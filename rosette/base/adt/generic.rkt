#lang racket

(require (only-in "../core/union.rkt" union union-filter union-guards union-contents)
         (only-in "../core/type.rkt" subtype?)
         (only-in "../core/bool.rkt" ||)
         (only-in "../core/safe.rkt" assert argument-error))

(provide adt-type-cast)

; This macro takes the form:
; * (adt-type-cast value #:type racket-type? #:lifted symbolic-type? #:caller caller)  
; The form expands into an expression that casts the given 
; value to the type specified by the primitive Racket 
; predicate and its corresponding lifted Rosette type?.  The cast 
; asserts a @boolean? that is true iff the cast is valid,  
; and it returns the result of casting the input value to symbolic-type?.   
; This macro assumes that the only possible non-concrete value of 
; such a type is a symbolic union.
(define-syntax-rule (adt-type-cast v #:type adt-type? #:lifted symbolic-type? #:caller caller)
  (match v
    [(? adt-type?) v]
    [(union xs t)
     (cond [(subtype? t symbolic-type?) v]
           [(subtype? symbolic-type? t)
            (match (union-filter v symbolic-type?)
              [(union (list (cons g u)) _)
               (assert g (argument-error caller (~a adt-type?) v))
               u]
              [u
               (unless (= (length xs) (length (union-contents u)))
                 (assert (apply || (union-guards u)) (argument-error caller (~a adt-type?) v)))
               u])]
           [else (assert #f (argument-error caller (~a adt-type?) v))])]
    [_ (assert #f (argument-error caller (~a adt-type?) v))]))