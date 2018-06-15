#lang rosette

(require rackunit rackunit/text-ui racket/generator
         rosette/lib/roseunit)

(current-bitwidth #f)
(define-symbolic x y z (bitvector 8))
(define-symbolic a b c boolean?)

(define (check-model sol m)
  (check-pred sat? sol)
  (check-equal? (model sol) m))

(define push-pop-tests
  (test-suite+ "Tests for the push / pop interface."
   #:features '(qf_bv)
         
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

  (solver-assert solver (list (equal? (bvadd x y) (bv 10 8)) (equal? (bvadd x (bvmul (bv 2 8) y)) (bv 20 8))))
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8)))
  (solver-push solver)
  (solver-assert solver (list (equal? (bvadd (bvmul (bv 3 8) x) y) (bv 10 8)) (equal? (bvadd (bvmul (bv 2 8) x) (bvmul (bv 2 8) y)) (bv 21 8))))
  (check-pred unsat? (solver-check solver))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8)))

  (solver-push solver)
  (solver-assert solver (list a (! b) c))
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8) a #t b #f c #t))

  (solver-pop solver)
  (check-pred sat? (solver-check solver))
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8)))

  (solver-push solver)
  (solver-assert solver (list (! a)))
  (solver-push solver)
  (solver-assert solver (list b))
  (solver-push solver)
  (solver-assert solver (list (! c)))
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8) a #f b #t c #f))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8) a #f b #t))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8) a #f))

  (solver-pop solver)
  (check-model (solver-check solver) (hash x (bv 0 8) y (bv 10 8)))
  
  ))

(module+ test
  (time (run-tests push-pop-tests)))
