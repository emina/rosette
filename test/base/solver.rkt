#lang racket

(require rosette/solver/smt/z3 rosette/solver/solver)

(provide solver solve solver-shutdown)

(define solver (make-parameter (z3)))

(define (solve  . asserts)
  (solver-assert (solver) asserts)
  (begin0
    (solver-check (solver))
    (solver-clear (solver))))

