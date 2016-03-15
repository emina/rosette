#lang racket

(require rosette/solver/solution 
         (only-in rosette symbolics expression = bveq <=>)
         (only-in rosette/query/debug debug-origin)
         rosette/lib/util/syntax (only-in racket/syntax format-id)
         slideshow/code (only-in slideshow vl-append current-font-size))
         
(provide render)

(define (render sol [font-size 16])
  (match (core sol)
    [(or #f (list #f)) #f]
    [uc
      (let* ([uc (filter-map location (relaxers uc))]
             [debugged (debugged-forms (remove-duplicates (map location-source uc)) uc)]
             [uc (apply set uc)])
        (parameterize ([code-colorize-enabled #t]
                       [current-font-size font-size])
          (apply vl-append (for/list ([stx debugged])
                         (eval #`(code #,(colorize-code stx uc)))))))]))

(define (relaxers cs)
  (define rs (mutable-set))
  (define seen? (mutable-set))
  (define (extract-relaxers e)
    (unless (set-member? seen? e)
      (match e
        [(app debug-origin (and (not (? false?)) origin))
         (set-add! rs origin)]
        [(expression (or (== =) (== bveq) (== <=>)) (app debug-origin origins) ...)
         (for ([origin origins] #:when origin)
           (set-add! rs origin))]
        [(expression _ es ...)
         (for ([e es]) (extract-relaxers e))]
        [_ (void)])))
  (for ([c cs]) (extract-relaxers c))
  (set->list rs))

(define (debugged-forms paths core)
  (define (debugged? loc)
    (for/or ([core-loc core])
      (location-contains? loc core-loc)))
  (apply 
   append
   (for/list ([source (map read-module paths)])
     (syntax-case source () 
       [(mod id lang (mod-begin forms ...))
        (for/list ([form (syntax->list #'(forms ...))]
                   #:when (debugged? (location form)))
          form)]
       [_ (error 'debugged-forms "expected a module, given ~a" source)]))))
  

(define (colorize-code stx core)
  (syntax-case stx ()
    [(id rest ...)
     (and (identifier? #'id) (free-label-identifier=? #'id #'quote))
     stx]
    [(expr rest ...)
     (let* ([children (for/list ([e (syntax->list stx)]) (colorize-code e core))]
            [children (datum->syntax stx children stx)]
            [color (if (or (set-member? core (location #'expr)) 
                           (set-member? core (location stx))) 
                       "red" "gray")])
       (quasisyntax/loc stx (colorize #,color #,children)))]
    [_ (let ([leaf (if (identifier? stx) (reformat-identifier stx) stx)])
         (if (set-member? core (location stx)) 
             (quasisyntax/loc stx (colorize "red" #,leaf))
             leaf))]))
         

(define-syntax colorize 
  (make-code-transformer 
   (syntax-rules () 
     [(_ color e) 
      (parameterize ([code-colorize-enabled #t]
                     [current-base-color color]
                     [current-comment-color color]
                     [current-keyword-color color]
                     [current-id-color color]
                     [current-literal-color color]
                     [current-const-color color])
        (code e))])))

(define (reformat-identifier stx)
  (format-id stx "~a" 
             (string->symbol (~s (syntax->datum stx)))
             #:source stx
             #:props stx))
