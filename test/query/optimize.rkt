#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define (minimize obj . asserts)
  (optimize #:minimize (list obj)
            #:guarantee (for ([a asserts]) (assert a))))

(define (maximize obj . asserts)
  (optimize #:maximize (list obj)
            #:guarantee (for ([a asserts]) (assert a))))

(define (check-solution actual expected [test =])
  (check-sat actual)
  (define ma (model actual))
  (define me (model expected))
  (check-equal? (dict-keys ma) (dict-keys me))
  (for ([(k v) ma])
    (check test v (dict-ref me k))))

(define basic-tests
  (test-suite+ 
   "Basic optimize tests with no finitization."
   #:features '(qf_lia optimize)
   (current-bitwidth #f)
   (define-symbolic x y integer?)
   (check-solution (maximize (+ x y) (< x 2) (< (- y x) 1))
                   (sat (hash x 1 y 1)))
   (check-unsat (maximize (+ x y) (< x 2) (< y 2) (> (+ x y) 4)))
   (check-unsat (minimize (+ x y) (< x 2) (< y 2) (> (+ x y) 4)))
   (check-solution (minimize (+ x y) (< x 4) (< (- y x) 1) (> y 1))
                   (sat (hash x 2 y 2)))
   (define-symbolic r q real?)
   (check-solution (maximize (+ r q) (< r 4) (< q 5))
                   (sat (hash r 3.0 q 4.0)) >=)
   ))

(module+ test
  (time (run-tests basic-tests)))
