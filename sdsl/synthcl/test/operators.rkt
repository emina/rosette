#lang s-exp rosette

(require rackunit rackunit/text-ui 
         (prefix-in cl/ "../model/operators.rkt") 
         "../model/reals.rkt")

(define-symbolic a b c boolean?)
(define-symbolic x y z number?)

(define vector-tests
  (test-suite 
   "Tests for vector operators"
   #:before (lambda () (printf "Testing vector operators\n"))
   
   (check-equal? (cl/apply-operator int3 cl/+ (if b (int3 0 1 2) (int3 x y z)) (int3 4 5 6))
                 (int3 (+ 4 (if b 0 x)) (+ 5 (if b 1 y)) (+ 6 (if b 2 z))))
   
   (check-equal? (cl/apply-operator int2 cl// (int2 x y) (int2 1 0))
                 (int2 x 0))
   
   (check-equal? (cl/apply-operator int2 cl/% (int2 x y) (int2 1 0))
                 (int2 0 0))
   
   ))

(time (run-tests vector-tests))