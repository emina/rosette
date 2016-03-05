#lang rosette

(define-symbolic b boolean?)
(define v (vector 1))
(define w (vector 2 3))
(define s (if b v w))
s
(type-of s)
(eq? s v)
(eq? s w)
(define u (if b '(1 2) 3))
u
(type-of u)

(define (test)
  (define-symbolic c boolean?)
  (define v (if c #t 0))
  (define u (if b (vector v) 4))
  (list v u))

(test)

(union-contents u)
