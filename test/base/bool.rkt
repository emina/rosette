#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term
         rosette/base/core/bool
         (only-in rosette/base/form/define define-symbolic)
         "common.rkt")

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

(define (check-logic op co id)
  (check-nary op id a b c)
  (check-equal? (op a (! a)) (! id))
  (check-equal? (op a (! id)) (! id))
  (check-equal? (op (co a b) (! (co a b))) (! id))
  (check-equal? (op (op a b) (! (op a b))) (! id))
  (check-equal? (op a (! id)) (! id))
  (check-equal? (! (! (op a b))) (op a b))
  (check-equal? (op c b a a a a b a c c ) (op a b c))
  (check-equal? (op c b a a a a b a (! c) c ) (! id))
  (check-equal? (op c (co c a) (co c b) a (co a b)) (op a c))
  (check-equal? (op (co a b) (co a b c)) (co a b))
  (check-equal? (op (co a b c) (co a b)) (co a b))
  (check-equal? (op (co a b) (co a b c) a) a)
  (check-equal? (op (co a b c) (co a b) a) a))

(define bool-tests
  (test-suite+ 
   "Tests for rosette/base/bool.rkt" 
   #:features '(qf_uf)
   
   (check-equal? (! (! a)) a)
   (check-equal? (! #t) #f)
   (check-equal? (! #f) #t)

   (check-logic && || #t)
   (check-logic || && #f)
   
   (check-true (<=> #t #t))
   (check-true (<=> #f #f))
   (check-false (<=> #t #f))
   (check-false (<=> #f #t))
   
   (check-true (<=> a a))
   (check-false (<=> a (! a)))
   (check-equal? (<=> a #t) a)
   (check-equal? (<=> a #f) (! a))
   (check-equal? (<=> a b) (<=> b a))
   (check-false (boolean? (<=> a b) ))))

(module+ test
  (time (run-tests bool-tests)))
