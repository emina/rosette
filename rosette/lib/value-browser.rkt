#lang racket/gui

(provide render-value/snip render-value/window)
(require mrlib/expandable-snip
         (only-in rosette bv? expression constant)
         (only-in rosette/base/core/term
                  get-type type-deconstruct typed?)
         (only-in rosette/base/core/union
                  union))

(define snip-class (new snip-class%))

;; the term browser for DrRacket users
(define (render-value/snip in #:handler [handler (thunk* #f)])
  (define (browse-term* in)
    (new term-snip% [in in]))

  (define term-snip%
    (class expandable-snip%
      (init-field in)

      (define summary-t (new text:hide-caret/selection%))
      (define inner-t (new text:hide-caret/selection%))
      (define snips '())
      (define texts '())
      (define has-set? #f)
      (define latest-style #f)

      (define/override (update-style-list sl)
        (set! latest-style sl)
        (super update-style-list sl)
        (send summary-t set-style-list sl)
        (send inner-t set-style-list sl)
        (for ([snip (in-list snips)])
          (send snip update-style-list sl))
        (for ([text (in-list texts)])
          (send text set-style-list sl)))

      (send summary-t insert (format "~.v" in))

      (define/private (make-inner)
        (match in
          [(constant id type)
           `([(emph "Kind: ") "constant"]
             [(emph "Id: ")   ,(~a in)]
             [(emph "Type: ") ,(~a type)])]
          [(union gvs)
           `([(emph "Kind: ") "union"]
             ,@(append*
                (for/list ([child (in-list gvs)])
                  `(#:gap
                    [,(browse-term* (car child))]
                    [,(browse-term* (cdr child))]))))]
          [(expression op child ...)
           `([(emph "Kind: ") "expression"]
             [(emph "Op: ")   ,(~a op)]
             ,@(for/list ([child (in-list child)] [i (in-naturals)])
                 `[,(~a i " ") ,(browse-term* child)]))]
          [(list child ...)
           `([(emph "Kind: ") "list"]
             ,@(for/list ([child (in-list child)] [i (in-naturals)])
                 `[,(~a i " ") ,(browse-term* child)]))]
          [(vector child ...)
           `([(emph "Kind: ") "vector"]
             ,@(for/list ([child (in-list child)] [i (in-naturals)])
                 `[,(~a i " ") ,(browse-term* child)]))]
          [(? box?)
           `([(emph "Kind: ") ,(if (immutable? in)
                                   "immutable-box"
                                   "box")]
             [(emph "Value: ") ,(browse-term* (unbox in))])]
          [(cons a b)
           `([(emph "Kind: ")  "cons"]
             [(emph "Left: ")  ,(browse-term* a)]
             [(emph "Right: ") ,(browse-term* b)])]

          [(? integer?)
           `([(emph "Kind: ")  "integer"]
             [(emph "Value: ") ,(~v in)])]
          [(? real?)
           `([(emph "Kind: ")  "real"]
             [(emph "Value: ") ,(~v in)])]
          [(? boolean?)
           `([(emph "Kind: ")  "boolean"]
             [(emph "Value: ") ,(~v in)])]

          [(? typed?)
           (define t (get-type in))
           (match (type-deconstruct t in)
             [(list (== in))
              ;; typed value
              (match in
                [(? bv?)
                 `([(emph "Kind: ")  "bitvector"]
                   [(emph "Value: ") ,(~v in)])]
                [(? procedure?)
                 `([(emph "Kind: ")  "computed procedure"]
                   [(emph "Value: ") ,(~v in)])]
                [_
                 ;; this should be a dead code in principle
                 `([(emph "Kind: ")  "typed"]
                   [(emph "Type: ")  ,(~a t)]
                   [(emph "Value: ") ,(~v in)])])]
             [vs
              `([(emph "Kind: ") "struct"]
                [(emph "Name: ") ,(~a t)]
                ,@(for/list ([v (in-list vs)])
                    `[,(browse-term* v)]))])]

          ;; a struct could have prop:procedure, so this test should
          ;; follow the struct test
          [(? procedure?)
           `([(emph "Kind: ")  "procedure"]
             [(emph "Value: ") ,(~v in)])]

          [_ (or (handler in browse-term*)
                 `([(emph "Kind: ")  "other"]
                   [(emph "Value: ") ,(~v in)]))]))

      (change-style summary-t 'change-family 'modern)

      (send summary-t lock #t)
      (send inner-t lock #t)

      (super-new
       [closed-editor summary-t]
       [open-editor inner-t]
       [layout 'replace]
       [callback (λ (open?)
                   (when (and open? (not has-set?))
                     (set! has-set? #t)
                     (send inner-t begin-edit-sequence)
                     (send inner-t lock #f)

                     (do-print (make-inner))
                     (change-style inner-t 'change-family 'modern)

                     (send inner-t lock #t)
                     (send inner-t end-edit-sequence)))])

      (define/private (do-text t)
        (define the-text (new text:hide-caret/selection%))
        (define container-snip (new editor-snip%
                                    [editor the-text]
                                    [left-margin 0]
                                    [top-margin 0]
                                    [right-margin 0]
                                    [bottom-margin 0]
                                    [left-inset 0]
                                    [top-inset 0]
                                    [right-inset 0]
                                    [bottom-inset 0]
                                    [with-border? #f]))
        (send inner-t insert container-snip)
        (send the-text insert t)
        (set! texts (cons the-text texts))
        (when latest-style
          (send the-text set-style-list latest-style))
        the-text)

      (define/private (do-row row)
        (cond
          [(equal? '#:gap row)
           (define the-pos (send inner-t last-position))
           (send inner-t insert "\n")
           (send inner-t change-style
                 (make-object style-delta% 'change-size 4)
                 the-pos (add1 the-pos))]
          [else
           (for ([col (in-list row)])
             (match col
               [_
                #:when (is-a? col snip%)
                (send inner-t insert col)
                (set! snips (cons col snips))]
               [`(emph ,x) (change-style (do-text x) 'change-bold)]
               [_ (do-text col)]))]))

      (define/private (do-print table)
        (match table
          ['() (void)]
          [(list first-row rest-rows ...)
           (do-row first-row)
           (for ([row (in-list rest-rows)])
             (send inner-t insert "\n")
             (do-row row))]))

      ;; Following are hacks to make expandable-snip% works on the REPL
      ;; See https://github.com/racket/gui/issues/157

      (define/override (copy) (browse-term* in))
      (define/override (write _) (void))

      (inherit set-snipclass)
      (set-snipclass snip-class)))

  (browse-term* in))

;; the term browser for CLI users
(define (render-value/window in #:handler [handler (thunk* #f)])
  (define es (render-value/snip in #:handler handler))
  (define f (new frame%
                 [label "Term Browser"]
                 [width 850]
                 [height 500]))
  (define t (new text:hide-caret/selection%))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (send t insert es)
  (send t lock #t)
  (send f show #t))

;; a textbox class that hides caret
(define text:hide-caret/selection%
  (class text%
    (inherit get-start-position get-end-position hide-caret)
    (define/augment (after-set-position)
      (hide-caret (= (get-start-position) (get-end-position))))
    (super-new)))

(define (change-style text . xs)
  (send text change-style
        (apply make-object style-delta% xs)
        0
        (send text last-position)))
