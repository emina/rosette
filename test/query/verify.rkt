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
    (check-verify unsat? (verify (begin (assume x) (assert (or x (not x))))))
    (check-verify sat? (verify (begin (assume x) (assert (not x)))))
    
    (check-verify unsat? (verify (assert (or (>= n 0) (< n 0)))))
    (check-verify sat? (verify (assert (= n 0))))
    (check-verify sat? (verify (assert (= (* n 2) 0))))
    (check-verify unsat? (verify (begin (assume (= n 0)) (= (* n 2) 0)))) ))

(define short-circuit-tests
  (test-suite+ "Verify short-circuit tests"

    (check-verify unsat? (verify (assert #t)))
    (check-verify unsat? (verify (begin (assume #f) (assert x))))    ; #f => x  is valid
    (check-verify sat?   (verify (begin (assume #t) (assert #f))))   ; #t => #f is invalid
    

    (check-verify unsat? (verify (begin (assume (and (= (+ (* 2 n) 1) 0) (not (= n 0))))
                                        (assert #f))))
    (check-verify unsat? (verify (begin (assume (and (= (+ (* 2 n) 1) 0) (not (= n 0))))
                                        (assert #t))))

    (check-verify unsat? (verify (begin (assume #f)
                                        (assert (and (= (+ (* 2 n) 1) 0) (not (= n 0)))))))
    (check-verify sat? (verify (begin (assume #t)
                                      (assert (and (= (+ (* 2 n) 1) 0) (not (= n 0)))))))))

(define (r0)
  (define-symbolic y boolean?)
  (define expression
    (+ (if x (if y 0.5 0) 1) (if y 0.5 0)))

  (define evaluated
    (evaluate expression (sat (hash x #t))))

  (verify (assert (= 0 evaluated))))
  
  
(define regression-tests
  (test-suite+ "Verify regression tests."
    (check-not-exn r0)))

(module+ test
  (time (run-tests verify-tests))
  (time (run-tests short-circuit-tests))
  (time (run-tests regression-tests)))
