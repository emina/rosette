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
  (define (browse-term** in)
    (new term-snip% [in in]))

  (define term-snip%
    (class expandable-snip%
      (init-field in)

      (define summary-t (new text:hide-caret/selection%))
      (define inner-t (new text:hide-caret/selection%))
      (define snips '())

      (define (browse-term* in)
        (define snip (browse-term** in))
        (set! snips (cons snip snips))
        snip)

      (define/override (update-style-list sl)
        (super update-style-list sl)
        (send summary-t set-style-list sl)
        (send inner-t set-style-list sl)
        (for ([snip (in-list snips)])
          (send snip update-style-list sl)))

      (send summary-t insert (format "~.v" in))

      (send inner-t begin-edit-sequence)

      (do-print
       (match in
         [(constant id type)
          `([(emph "Kind:") "constant"]
            [(emph "Id:") ,(~a in)]
            [(emph "Type:") ,(~a type)])]
         [(union gvs)
          `([(emph "Kind:") "union"]
            ,@(append*
               (for/list ([child (in-list gvs)])
                 `(#:gap
                   [,(browse-term* (car child))]
                   [,(browse-term* (cdr child))]))))]
         [(expression op child ...)
          `([(emph "Kind:") "expression"]
            [(emph "Op:") ,(~a op)]
            ,@(for/list ([child (in-list child)])
                `[,(browse-term* child)]))]
         [(list child ...)
          `([(emph "Kind:") "list"]
            ,@(for/list ([child (in-list child)])
                `[,(browse-term* child)]))]
         [(vector child ...)
          `([(emph "Kind:") "vector"]
            ,@(for/list ([child (in-list child)])
                `[,(browse-term* child)]))]
         [(? box?)
          `([(emph "Kind:") ,(if (immutable? in)
                                 "immutable-box"
                                 "box")]
            [(emph "Value:") ,(browse-term* (unbox in))])]
         [(cons a b)
          `([(emph "Kind:")  "cons"]
            [(emph "Left:")  ,(browse-term* a)]
            [(emph "Right:") ,(browse-term* b)])]


         [(? bv?)
          `([(emph "Kind:")  "bitvector"]
            [(emph "Value:")  ,(~v in)])]
         [(? number?)
          `([(emph "Kind:")  "number"]
            [(emph "Value:")  ,(~v in)])]
         [(? boolean?)
          `([(emph "Kind:")  "boolean"]
            [(emph "Value:")  ,(~v in)])]
         [(? procedure?)
          `([(emph "Kind:")  "procedure"]
            [(emph "Value:")  ,(~v in)])]


         [(? typed?)
          (let ([t (get-type in)])
            (match (type-deconstruct t in)
              [(list (== in))
               `([(emph "Kind:") "typed"]
                 [(emph "Value:") ,(~v in)])]
              [vs
               `([(emph "Kind:") "struct"]
                 [(emph "Name:") ,(~a (get-type in))]
                 ,@(for/list ([v (in-list vs)])
                     `[,(browse-term* v)]))]))]
         [_ (or (handler in browse-term*)
                `([(emph "Kind:") "other"]
                  [(emph "Value:") ,(~v in)]))]))

      (send inner-t end-edit-sequence)

      (make-modern summary-t)
      (make-modern inner-t)

      (send summary-t lock #t)
      (send inner-t lock #t)

      (super-instantiate
       ()
       [closed-editor summary-t]
       [open-editor inner-t]
       [layout 'replace])

      (define/private (do-row row)
        (cond
          [(equal? '#:gap row)
           (define the-pos (send inner-t last-position))
           (send inner-t insert "\n ")
           (send inner-t change-style
                 (make-object style-delta% 'change-size 1)
                 the-pos (add1 the-pos))]
          [else
           (for ([col (in-list row)])
             (define the-pos (send inner-t last-position))
             (match col
               [`(emph ,x) (send inner-t insert x)]
               [_ (send inner-t insert col)])
             (send inner-t insert " ")
             (match col
               [`(emph ,_)
                (send inner-t change-style
                      (make-object style-delta% 'change-bold)
                      the-pos
                      (sub1 (send inner-t last-position)))]
               [_ (void)]))]))

      (define/private (do-print table)
        (do-row (first table))
        (for ([row (in-list (rest table))])
          (send inner-t insert "\n")
          (do-row row)))

      ;; Following are hacks to make expandable-snip% works on the REPL
      ;; See https://github.com/racket/gui/issues/157

      (define/override (copy) (browse-term** in))
      (define/override (write _) (void))

      (inherit set-snipclass)
      (set-snipclass snip-class)))

  (browse-term** in))

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

;; make teletype font
(define (make-modern text)
  (send text change-style
        (make-object style-delta% 'change-family 'modern)
        0
        (send text last-position)))
