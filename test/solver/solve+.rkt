#lang racket

(require rackunit rackunit/text-ui
         rosette/config/config rosette/query/eval
         rosette/base/term rosette/base/bool rosette/base/num
         rosette/solver/solution
         (only-in rosette/base/define define-symbolic))

(provide run-tests-with)

(define solver #f)

(define-symbolic x @number?)
(define-symbolic y @number?)
(define-symbolic z @number?)

(define-symbolic v @number?)
(define-symbolic u @number?)

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

(define (check-sat . formulas)
  (send/apply solver assert formulas)
  (let ([sol (send solver solve)])
    ;(printf "~s\n" formulas)
    (check-true (sat? sol) (format "no solution found for ~s" formulas))
    (for ([f formulas])
      (check-equal? #t (evaluate f sol) (format "solution violates ~a: ~a" f sol)))))

(define (check-unsat . formulas)
  (send/apply solver assert formulas)
  (let ([sol (send solver solve)])
    ;(printf "~s\n" formulas)
    (check-true (unsat? sol) (format "bad solution found for ~a: ~a" formulas sol))))

(define bool-tests
  (test-suite
   "incremental solving"
   #:before (lambda () (printf "Testing incremental solving with ~a\n" solver))
   (check-sat (|| a b))
   (check-sat (|| b c))
   (check-sat (|| a (! c)))
   (check-sat (&& a b c))
   (check-unsat (! a))
   (send solver clear)
   (check-sat (! a))
   ))

(define (run-tests-with s)
  (set! solver s)
  (time (run-tests bool-tests))
  (send solver shutdown))
  
