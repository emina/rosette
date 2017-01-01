#lang racket

(provide (all-defined-out))

(struct objective (type expr))
(struct bound (var lb ub))

; Given a symbolic variable, return its name excluding $ symbol.
; Excluding $ because it is invalid in CPLEX.
(define (get-name v)
  (define name (format "~a" v))
  (string-replace name "$" ""))