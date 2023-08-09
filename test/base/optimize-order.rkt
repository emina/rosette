#lang rosette

;; This test is taken from https://www.philipzucker.com/z3-rise4fun/optimization.html

(require rackunit rackunit/text-ui racket/generator
         rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic x y z integer?)

(define (check-model sol m)
  (check-pred sat? sol)
  (check-equal? (model sol) m))

(define optimization-order-tests
  (test-suite+ "Tests for the optimization order"
   #:features '(optimize)

  (define solver (current-solver))

  (solver-assert solver (list (< x z) (< y z) (< z 5) (not (= x y))))
  (solver-maximize solver (list x))
  (solver-maximize solver (list y))
  (check-model (solver-check solver) (hash x 3 y 2 z 4))))

(module+ test
  (time (run-tests optimization-order-tests)))
