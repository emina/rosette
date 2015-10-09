#lang racket

(require "../../solver/solution.rkt" "../../base/core/term.rkt" 
         rosette/lib/util/syntax (only-in racket/syntax format-id)
         slideshow/code (only-in slideshow vl-append current-font-size))
         
(provide render)

(define (render sol [font-size 16])
  (unless (unsat? sol)
    (error 'render "expected an unsatisfiable solution, given ~a" sol))
  (let* ([core (filter-map location (remove-duplicates (filter-map term-origin (core sol))))]
         [debugged (debugged-forms (remove-duplicates (map location-source core)) core)]
         [core (apply set core)])
    (parameterize ([code-colorize-enabled #t]
                   [current-font-size font-size])
      (apply vl-append (for/list ([stx debugged])
                         (eval #`(code #,(colorize-code stx core))))))))

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
