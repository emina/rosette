#lang racket

(require (only-in "type.rkt" typed? get-type types least-common-supertype))
(require "any.rkt")

(provide type-of)


; Returns a type t that accepts the given values, and there is no t' 
; such that t' != t, (subtype? t' t), and t' also accepts the given values. 
; the behavior of this function is undefined if no type accepts all values.
; a type accepts a value v iff (type v) is #t.
(define type-of
  (case-lambda 
    [(v)   (if (typed? v)
               (get-type v)
               (for/first ([t types] #:when (t v)) t))]
    [(v u) (least-common-supertype (type-of v) (type-of u))]
    [vs    (for/fold ([t (type-of (car vs))]) ([v (cdr vs)] #:break (eq? t @any?))
             (least-common-supertype t (type-of v)))]))



