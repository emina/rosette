#lang racket

(require (only-in "../core/union.rkt" union union-filter union-guards)
         (only-in "../core/type.rkt" subtype?)
         (only-in "../core/bool.rkt" ||))
(provide adt-cast)

; This macro takes two forms:
; * (adt-cast value #:type racket-type? #:lifted symbolic-type?) 
; * (adt-cast #:type racket-type? #:lifted symbolic-type?) 
; The first form expands into an expression that casts the given 
; value to the type specifiedy by the primitive Racket 
; predicate and its corresponding lifted Rosette type?.  The cast 
; returns two values: a @boolean? that is true iff the cast is valid 
; and the result of casting the input value to symbolic-type?.   
; The second form expands into a procedure that takes as input a value 
; and then performs this cast.  
; This macro assumes that the only possible non-concrete value of 
; such a type is a symbolic union.
(define-syntax adt-cast
  (syntax-rules ()
    [(_  v #:type adt-type? #:lifted symbolic-type?)
     (match v
         [(? adt-type?) (values #t v)]
         [(union _ t)
          (cond [(subtype? t symbolic-type?) (values #t v)]
                [(subtype? symbolic-type? t)
                 (match (union-filter v symbolic-type?)
                   [(union (list) _) (values #f v)]
                   [(union (list (cons g u)) _) (values g u)]
                   [u (values (apply || (union-guards u)) u)])]
                [else (values #f v)])]
         [_ (values #f v)])]
    [(_  #:type adt-type? #:lifted symbolic-type?)
     (lambda (v) (adt-cast v #:type adt-type? #:lifted symbolic-type?))]))
  
