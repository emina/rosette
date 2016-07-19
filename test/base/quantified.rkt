#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term rosette/base/core/bool)

(define-syntax-rule (check-state actual expected-value expected-asserts)
  (let-values ([(e ignore) (with-asserts expected-value)]
               [(v a) (with-asserts actual)])
    (check-equal? v e)
    (check-equal? (apply set a) (apply set expected-asserts))))

(define (check-pe op)
  (define-symbolic a boolean?)
  (define-symbolic b integer?)
  (define-symbolic c real?)
  (define-symbolic f (~> integer? real?))
  (check-exn #px"primitive solvable types" (thunk (op a #t))) ; not a list
  (check-exn #px"primitive solvable types" (thunk (op (list f) #t))) ; not solvable
  (check-exn #px"primitive solvable types" (thunk (op (list b 1) #t))) ; not a constant
  (check-exn #px"boolean\\?" (thunk (op (list) 1))) ; not boolean body
  (check-exn #px"boolean\\?" (thunk (op (list b c) 1))) ; not boolean body
  (clear-asserts!)
  (check-state (op (list a b c) #t) #t (list)) ; constant body
  (check-state (op (list a b c) #f) #f (list)) ; constant body
  (check-state (op (list) a) a (list)) ; empty list of quantified variables
  (check-state (op (list b c) (= 3 (+ b c))) (expression op (list b c) (= 3 (+ b c))) (list))
  (check-state (op (list b c) (if a #t 1)) #t (list a))
  (check-state (op (list) (if a #t 1)) #t (list a))
  (check-state (&& a (op (list b c) (= 3 (+ b c))))
               (&& a (expression op (list b c) (= 3 (+ b c)))) (list))
  (check-state (op (list b) (op (list c) (= 3 (+ b c))))
               (expression op (list b)
                           (expression op (list c)
                                       (= 3 (+ b c))))
               (list)))

(define tests:basic
  (test-suite+
   "Basic tests for quantified formulas"
   (current-bitwidth #f)
   (check-pe exists)
   (check-pe forall)))

(time (run-tests tests:basic))