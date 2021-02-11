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

(require rosette/lib/angelic)

(define (vector-update! v idx proc)
  (vector-set! v idx (proc (vector-ref v idx))))

(define (vector-update!! v idx proc)
  (for/all ([idx idx #:exhaustive])
    (vector-update! v idx proc)))

(define limit 10000)

(define slow (list->vector (build-list limit identity)))
(define fast (list->vector (build-list limit identity)))

(define idx (choose* 0 5 10))

(time (vector-update! slow idx add1))
(vector-ref slow 0)
(vector-ref slow 1)

(time (vector-update!! fast idx add1))
(vector-ref fast 0)
(vector-ref fast 1)