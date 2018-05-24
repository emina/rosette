#lang rosette

(require rackunit rackunit/text-ui racket/generator
         rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic x y z integer?)
(define-symbolic a b c boolean?)

(define (check-model sol m)
  (check-pred sat? sol)
  (check-equal? (model sol) m))

(define push-pop-tests
  (test-suite+ "Tests for the push / pop interface."
   #:features '(qf_lia)
         
  (define solver (current-solver))
  (check-exn exn:fail? (thunk (solver-pop solver)))

  (solver-push solver)
  (solver-pop solver)
  (solver-push solver)
  (solver-push solver)

  (check-exn exn:fail? (thunk (solver-pop solver -1)))
  (check-exn exn:fail? (thunk (solver-pop solver 0)))
  (check-exn exn:fail? (thunk (solver-pop solver 3)))
  (solver-pop solver 2)

  (solver-assert solver (list (= (+ x y) 10) (= (+ x (* 2 y)) 20)))
  (check-model (solver-check solver) (hash x 0 y 10))
  (solver-push solver)
  (solver-assert solver (list (= (+ (* 3 x) y) 10) (= (+ (* 2 x) (* 2 y)) 21)))
  (check-pred unsat? (solver-check solver))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x 0 y 10))

  (solver-push solver)
  (solver-assert solver (list a (! b) c))
  (check-model (solver-check solver) (hash x 0 y 10 a #t b #f c #t))

  (solver-pop solver)
  (check-pred sat? (solver-check solver))
  (check-model (solver-check solver) (hash x 0 y 10))

  (solver-push solver)
  (solver-assert solver (list (! a)))
  (solver-push solver)
  (solver-assert solver (list b))
  (solver-push solver)
  (solver-assert solver (list (! c)))
  (check-model (solver-check solver) (hash x 0 y 10 a #f b #t c #f))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x 0 y 10 a #f b #t))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x 0 y 10 a #f))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x 0 y 10))
  
  ))

(module+ test
  (time (run-tests push-pop-tests)))
