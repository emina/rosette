#lang racket

(require rosette/solver/smt/z3
         (prefix-in $ (only-in rosette/solver/solver assert solve clear shutdown)))

(provide solver solve shutdown)

(define solver (make-parameter (z3)))

(define (solve  . asserts)
  ($assert (solver) asserts)
  (begin0
    ($solve (solver))
    ($clear (solver))))

(define (shutdown) ($shutdown (solver)))