#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/query/debug)

(define-symbolic a b c boolean?)
(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)
(define-symbolic xb yb zb (bitvector 4))

(define/debug (x+1 x) (+ x 1))

(define basic-tests
  (test-suite+ 
   "Basic debug tests."
   #:features '(qf_lia unsat-cores)            
   (define sol (debug [integer?] (assert (= 1 (x+1 1)))))
   (check-unsat sol)
   (check-equal? (length (core sol)) 2)
   (check-exn exn:fail? (thunk (debug [integer?] (assert (= 1 (x+1 0))))))))

(module+ test
  (time (run-tests basic-tests)))
