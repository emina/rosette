#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)
(require rosette/solver/mip/cplex)

(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)

(define solver (cplex))
(current-solver solver)

(define basic-tests
  (test-suite+ "Solve basic tests."
    (with-handlers ([exn:fail?
                     (λ (e)
                       (unless (equal? (exn-message e) "MIP solver requires at least one objective.")
                         (raise e)))])
      (solve (assert #t)))
    
    (assert (> xi 0))
    (solver-minimize solver (list xi))
    (solver-assert solver (asserts))
    (check-sat (solver-check solver))
    
    (clear-asserts!)
    (assert (< xi 0)) ; in CPLEX, a variable is >= 0
    (solver-maximize solver (list xi))
    (solver-assert solver (asserts))
    (check-unsat (solver-check solver)) ; unsat
    
    (clear-asserts!)
    (assert (< xi 10))
    (solver-maximize solver (list xi))
    (solver-assert solver (asserts))
    (check-sat (solver-check solver))
    
    (clear-asserts!)
    (assert (< xi (+ xi yr)))
    (assert (= yr 2.5))
    (solver-minimize solver (list xi))
    (solver-assert solver (asserts))
    (check-sat (solver-check solver))
    ))


(define mip-tests
  (test-suite+ "Solve MIP tests."
    (clear-asserts!)
    (define obj (+ (* 2 xi) (* 3 yi)))
    (assert (<= (+ (* xi (/ 2 9)) (* yi (/ 1 4))) 1))
    (assert (<= (+ (* xi (/ 1 7)) (* yi (/ 1 3))) 1))
    (solver-maximize solver (list obj))
    (solver-assert solver (asserts))
    (define sol (check-sat (solver-check solver)))
    (check-equal? (evaluate xi sol) 2)
    (check-equal? (evaluate yi sol) 2)

    (clear-asserts!)
    (define obj2 (+ (* 18 xr) (* 3 yr) (* 9 zr)))
    (assert (<= (+ (* xr 2) (* yr 1) (* zr 7)) 160))
    (assert (<= xr 60))
    (assert (<= yr 30))
    (assert (<= zr 20))
    (solver-maximize solver (list obj2))
    (solver-assert solver (asserts))
    (define sol2 (check-sat (solver-check solver)))
    (check-equal? (evaluate xr sol2) 60)
    (check-equal? (evaluate yr sol2) 30)
    (check-true (and (> (evaluate zr sol2) 1.4) (< (evaluate zr sol2) 1.5)))
    ))

(define non-mip-tests
  (test-suite+ "Solve MIP tests."
    (clear-asserts!)
    (define obj (+ (* 2 xi) (* 3 yi)))
    (assert (<= (* (* xi (/ 2 9)) (* yi (/ 1 4))) 1))
    (assert (<= (+ (* xi (/ 1 7)) (* yi (/ 1 3))) 1))
    (solver-maximize solver (list obj))
    (solver-assert solver (asserts))
    (check-exn
     exn:fail?
     (lambda ()
       (solver-check solver)))
    ))


(define multi-objectives-tests
  (test-suite+ "Solve MIP multi objectives."
    (clear-asserts!)
    (define obj (+ (* 2 xi) (* 2 yi) (* 2 zi)))
    (assert (<= (+ xi yi zi) 1))
    (solver-maximize solver (list obj xi))
    (solver-assert solver (asserts))
    (define sol (check-sat (solver-check solver)))
    (check-equal? (evaluate xi sol) 1)
    (check-equal? (evaluate yi sol) 0)
    (check-equal? (evaluate zi sol) 0)))


(define multi-objectives-unsat-tests
  (test-suite+ "Solve MIP unsat multi objectives."
    (clear-asserts!)
    (define obj (+ (* 2 xi) (* 2 yi) (* 2 zi)))
    (assert (<= (+ xi yi zi) 1))
    (assert (>= xi 1))
    (assert (>= yi 1))
    (assert (>= zi 1))
    (solver-maximize solver (list obj xi))
    (solver-assert solver (asserts))
    (check-unsat (solver-check solver))))


(define distribution-tests
  (test-suite+ "Solve formula that requires the distribution rule."
    (clear-asserts!)
    (define obj (* 2 (+ xi yi zi)))
    (assert (<= (* 3 (+ xi yi zi)) 3))
    (solver-maximize solver (list obj zi))
    (solver-assert solver (asserts))
    (define sol (check-sat (solver-check solver)))
    (check-equal? (evaluate xi sol) 0)
    (check-equal? (evaluate yi sol) 0)
    (check-equal? (evaluate zi sol) 1)
    ))


(define mip-sol-start-tests
  (test-suite+ "Test mip-sol and mip-start options."
    (clear-asserts!)
    (define obj (+ (* 2 xi) (* 2 yi) (* 2 zi)))
    (assert (<= (+ xi yi zi) 1))
    (solver-maximize solver (list obj))
    (solver-assert solver (asserts))
    (define mip-sol-file (make-temporary-file "cplex-test-sol-~a.mst"))
    (solver-check-with-init solver #:mip-sol mip-sol-file)

    (solver-maximize solver (list obj xi))
    (solver-assert solver (asserts))
    (define sol (solver-check-with-init solver #:mip-start mip-sol-file))
    (check-equal? (evaluate xi sol) 1)
    (check-not-exn (thunk (delete-file mip-sol-file)))))

(time (run-tests basic-tests))
(time (run-tests mip-tests))
(time (run-tests non-mip-tests))
(time (run-tests multi-objectives-tests))
(time (run-tests multi-objectives-unsat-tests))
(time (run-tests distribution-tests))
(time (run-tests mip-sol-start-tests))