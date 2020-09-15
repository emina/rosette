#lang rosette

(define-symbolic b boolean?)

(define f
  (case-lambda
    [() (let-values ([(a b) 1])
          1)]
    [(a) (2)]))

(when b (f))
