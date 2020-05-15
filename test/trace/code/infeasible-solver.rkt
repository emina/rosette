#lang rosette

(define-symbolic n integer?)

(define xs (if (= n 0) '(1) '()))
(when (= (add1 n) 1)
  (apply add1 xs))
