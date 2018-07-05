#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         (prefix-in $ "../model/operators.rkt") 
         "../model/reals.rkt")

(define-symbolic a b c boolean?)
(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)

(define scalar-tests
  (test-suite+
   "Tests for scalar operators"

   (current-bitwidth 5)
   (check-equal? ($& 3 5 7) (bitwise-and 3 5 7))
   (check-equal? ($$ 3 5 7) (bitwise-ior 3 5 7))
   (check-equal? ($^ 3 5 7) (bitwise-xor 3 5 7))
   (check-equal? ($~ 12) (bitwise-not 12))
   (check-equal? ($<< 3 2) (arithmetic-shift 3 2))
   (check-equal? ($>> -3 2) (arithmetic-shift -3 -2))
   (check-equal? ($sqrt 3.4) (sqrt 3.4))
   (check-equal? ($sqrt 4) 2)
   (check-equal? ($sqrt -4) 0)
   
   (define sol (sat (hash xi 3 yi 5 zi 7)))
   (check-equal? (evaluate ($& xi yi zi) sol) (bitwise-and 3 5 7))
   (check-equal? (evaluate ($$ xi yi zi) sol) (bitwise-ior 3 5 7))
   (check-equal? (evaluate ($^ xi yi zi) sol) (bitwise-xor 3 5 7))
   
   (check-equal? (evaluate ($~ xi) (sat (hash xi 12))) (bitwise-not 12))
   (check-equal? (evaluate ($<< xi yi) (sat (hash xi 3 yi 2))) (arithmetic-shift 3 2))
   (check-equal? (evaluate ($>> xi yi) (sat (hash xi -3 yi 2))) (arithmetic-shift -3 -2))
   
   (check-equal? (evaluate ($sqrt xi) (sat (hash xi 9))) 3)
   (check-equal? (evaluate ($sqrt xr) (sat (hash xr 9.0))) 3)
   (check-equal? (evaluate ($sqrt xi) (sat (hash xi 5))) 2)
   (check-equal? (evaluate ($sqrt xr) (sat (hash xr 5.3))) 2)
   ))

(define vector-tests
  (test-suite+ 
   "Tests for vector operators"
   
   (check-equal? ($apply-operator int3 $+ (if b (int3 0 1 2) (int3 xi yi zi)) (int3 4 5 6))
                 (int3 (+ 4 (if b 0 xi)) (+ 5 (if b 1 yi)) (+ 6 (if b 2 zi))))
   
   (check-equal? ($apply-operator int2 $/ (int2 xi yi) (int2 1 0))
                 (int2 xi 0))
   
   (check-equal? ($apply-operator int2 $% (int2 xi yi) (int2 1 0))
                 (int2 0 0))
   
   ))

(time (run-tests scalar-tests))
(time (run-tests vector-tests))
