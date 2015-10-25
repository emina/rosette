#lang racket

(require rackunit rackunit/text-ui (only-in rosette/lib/util/roseunit test-suite+)
         rosette/query/eval rosette/query/state
         rosette/base/core/term rosette/base/core/bool rosette/base/core/num
         rosette/solver/solution
         (only-in rosette/base/form/define define-symbolic))

(provide run-tests-with)

(define-symbolic x @number?)
(define-symbolic y @number?)
(define-symbolic z @number?)

(define-symbolic v @number?)
(define-symbolic u @number?)

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

(define (check-sat . formulas)
  (send/apply (current-solver) assert formulas)
  (let ([sol (send (current-solver) solve)])
    ;(printf "~s\n" formulas)
    (check-true (sat? sol) (format "no solution found for ~s" formulas))
    (for ([f formulas])
      (check-equal? #t (evaluate f sol) (format "solution violates ~a: ~a" f sol)))))

(define (check-unsat . formulas)
  (send/apply (current-solver) assert formulas)
  (let ([sol (send (current-solver) solve)])
    ;(printf "~s\n" formulas)
    (check-true (unsat? sol) (format "bad solution found for ~a: ~a" formulas sol))))

(define bool-tests
  (test-suite+
   (format "Testing incremental solving with ~a\n" (current-solver))
   (check-sat (|| a b))
   (check-sat (|| b c))
   (check-sat (|| a (! c)))
   (check-sat (&& a b c))
   (check-unsat (! a))
   (send (current-solver) clear)
   (check-sat (! a))))

(define (run-tests-with s)
  (parameterize ([current-solver s])
    (time (run-tests bool-tests))
    (send (current-solver) shutdown)))
  
