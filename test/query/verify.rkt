#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic x boolean?)
(define-symbolic n integer?)

(define-syntax-rule (check-verify pred test)
  (let ([sol (with-handlers ([exn:fail? (const (unsat))])
               test)])
    (check-true (pred sol) (format "not ~a for ~a: ~a" (quote pred) (quote test) sol))))

(define verify-tests
  (test-suite+ "Basic verify tests"
    ; basic verify tests
    (check-verify unsat? (verify (assert (or x (not x)))))
    (check-verify sat? (verify (assert (and x (not x)))))
    (check-verify unsat? (verify #:assume (assert x) #:guarantee (assert (or x (not x)))))
    (check-verify sat? (verify #:assume (assert x) #:guarantee (assert (not x))))
    
    (check-verify unsat? (verify (assert (or (>= n 0) (< n 0)))))
    (check-verify sat? (verify (assert (= n 0))))
    (check-verify sat? (verify (assert (= (* n 2) 0))))
    (check-verify unsat? (verify #:assume (= n 0) #:guarantee (= (* n 2) 0)))
    ))

(define short-circuit-tests
  (test-suite+ "Verify short-circuit tests"
    ; tests for the verify short-circuit logic
    (check-verify unsat? (verify (assert #t)))
    (check-verify unsat? (verify #:assume (assert #f) 
                                 #:guarantee (assert x)))  ; #f => x  is valid
    (check-verify sat? (verify #:assume (assert #t) 
                               #:guarantee (assert #f)))   ; #t => #f is invalid
    
    ; unsat assumption that defeats simplification
    (check-verify unsat? (verify #:assume (assert (and (= (+ (* 2 n) 1) 0) (not (= n 0))))
                                 #:guarantee (assert #f)))
    (check-verify unsat? (verify #:assume (assert (and (= (+ (* 2 n) 1) 0) (not (= n 0))))
                                 #:guarantee (assert #t)))
    ; invalid guarantee that defeats simplification
    (check-verify unsat? (verify #:assume (assert #f)
                                 #:guarantee (assert (and (= (+ (* 2 n) 1) 0) (not (= n 0))))))
    (check-verify sat? (verify #:assume (assert #t)
                               #:guarantee (assert (and (= (+ (* 2 n) 1) 0) (not (= n 0))))))
    ))

(module+ test
  (time (run-tests verify-tests))
  (time (run-tests short-circuit-tests)))
