#lang rosette

(define-symbolic b boolean?)
(define (tri n acc)
  (cond
    [(zero? n) (1)]
    [else (tri (sub1 n) (+ acc n))]))

(when b (add1 (tri 500000 0)))
