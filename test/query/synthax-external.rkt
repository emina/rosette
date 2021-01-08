#lang rosette

(require rosette/lib/synthax)

(provide (all-defined-out))

; Use of holes that will be marshalled through bytecode.

(define (c0) (??))
(define (c1 x) (choose 1 (choose x 3)))
(define (c2 x) (choose 6 (+ x (c0)) 8))
(define (c3 x) (choose 1 (c2 x)))

(define-synthax (crec x k)
  (assert (>= k 0))
  (choose x (c0) (+ x (crec x (sub1 k)))))

(define-grammar (grec x)
  [s (choose x (c0) (+ x (s)))])

