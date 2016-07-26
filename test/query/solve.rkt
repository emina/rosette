#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic a b c boolean?)
(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)
(define-symbolic xb yb zb (bitvector 4))

(define basic-tests
  (test-suite+ "Solve tests with no (effective) finitization."
    (current-bitwidth #f)

    (check-sat (solve (assert #t)))
    (check-unsat (solve (assert #f)))
    
    (check-unsat (solve (assert (> xi (+ xi yr)))
                        (assert (= yr 2.5))))
    (check-sat (solve (assert (< xi (+ xi yr)))
                      (assert (= yr 2.5))))
    
    (check-unsat (solve (assert (> xi (+ xi (bitvector->integer yb))))
                        (assert (bveq yb (bv 1 4)))))    
    (check-sat (solve (assert (< xi (+ xi (bitvector->integer yb))))
                      (assert (bveq yb (bv 1 4)))))
    
    (check-sat (solve (assert (= (/ xr yr) zr))
                      (assert (= zr 1.5))))
    (check-sat (solve (assert (= (* xr yr) zr))
                      (assert (= zr 20000))))
    
    (current-bitwidth 4)
    (check-sat (solve (assert #t)))
    (check-unsat (solve (assert #f)))
    (check-sat (solve (assert (bveq xb yb))))
    (check-unsat (solve (assert (bveq xb yb))
                        (assert (bveq xb (bv 0 4)))
                        (assert (bveq yb (bv 1 4)))))
    ))

(define finitized-tests
  (test-suite+ "Solve tests with finitization."
    (current-bitwidth 5)
    
    ; Finite model that is also a real model.
    (check-sat (solve (assert (= (/ xr yr) zr))
                      (assert (= zr 2))))
    (check-sat (solve (assert (= xr (bitvector->integer yb)))))
    
    ; No finite model that is also a real model.
    (check-unsat (solve (assert (= (/ xr yr) zr))
                        (assert (= zr 1.5))))
    (check-unsat (solve (assert (= (* xr yr) zr))
                        (assert (= zr 20000))))
    
    (current-bitwidth 3)
    (check-unsat (solve (assert (= xr (bitvector->integer yb)))
                        (assert (bveq yb (bv -8 4)))))
 ))     

(define unknown-tests 
  (test-suite+ "Solve tests for unknown solutions."
    (parameterize ([current-bitwidth #f])
      (check-pred
       unknown?
       (solve
        (assert (> (* xi xi) 3))
        (assert (= (+ (* xr xr xr) (* xr yr)) 3.0)))))))
     
(define regression-tests 
  (test-suite+ "Solve regression tests."
    (parameterize ([current-bitwidth 32])
      (check-sat (solve (assert (<= 0 (abs (if (= 2 xi) 0 10)))))))))


(time (run-tests basic-tests))
(time (run-tests finitized-tests))
(time (run-tests regression-tests))
(time (run-tests unknown-tests))