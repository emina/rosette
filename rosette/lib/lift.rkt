#lang rosette

(require "match.rkt" (for-syntax (only-in racket/syntax generate-temporary))
         (only-in racket/splicing splicing-let))

(provide define-lift) 

; The define-lift macro lifts a Racket procedure to 
; work on symbolic union representations of ordinary Racket 
; values.  This means the lifted procedure will work when given 
; either a concrete Racket value or a symbolic union that 
; could evaluate to such a value.  Note that the lifted procedure 
; will not work on primitive symbolic values (boolean?, integer?, real? and 
; bitvector?), only on symbolic unions or concrete values.
;
; The define-lift macro takes takes one of two forms:
; * (define-lifted lifted-id [(arg-type ...) procedure-to-lift])
; * (define-lifted lifted-id [args-type procedure-to-lift])
;
; When the procedure to be lifted takes a specific number of arguments, 
; the first form should be used, and the type of each argument should be given.
; When the procedure to be lifted takes a variable number of arguments, 
; the type of all arguments should be given.  Note that the second form omits 
; the parentheses around the argument type to indicate a variable number of 
; arguments, just like Racket's case-lambda form.
(define-syntax (define-lift stx)
  (syntax-case stx ()
    [(_ id [type impl])
     (not (identifier? #'impl))
     (with-syntax ([tmp (generate-temporary)])
       (syntax/loc stx 
         (splicing-let ([tmp impl])
           (define-lift id [type tmp]))))]
    [(_ id [(type? ...)  impl])
     (with-syntax ([(arg ...) (generate-temporaries #'(type? ...))])
       (syntax/loc stx
         (define (id arg ...)
           (match* (arg ...)
             [((? type? arg) ...) (impl arg ...)]
             [(arg ...) (error 'id "expected ~a, given ~a" (list type? ...) (list arg ...))]))))]
    [(_ id [type?  impl])
     (syntax/loc stx
       (define id
         (case-lambda 
           [()    (impl)]
           [(x)   (match x 
                    [(? type? x) (impl x)])]
           [(x y) (match* (x y)
                    [((? type? x) (? type? y)) (impl x y)])]
           [xs    (id (car xs) (apply id (cdr xs)))])))]))

                  
  
