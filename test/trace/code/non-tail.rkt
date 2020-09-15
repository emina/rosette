#lang rosette

(define-symbolic b boolean?)
(define (tri n)
  (cond
    [(zero? n) (1)]
    [else (+ n (tri (sub1 n)))]))

(when b (add1 (tri 10)))
