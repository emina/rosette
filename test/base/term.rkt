#lang racket

(require rackunit rackunit/text-ui
         rosette/base/term
         rosette/base/bool
         rosette/base/num
         (only-in rosette/base/define define-symbolic))

(define-symbolic x @number?)
(define-symbolic y @number?)
(define-symbolic z @number?)

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

(define (check-ordered v1 v2)
  (check-true (and (or (term<? v1 v2) (term<? v2 v1)) 
                   (not (and (term<? v1 v2) (term<? v2 v1))))))

(define (check-cached op . args)
  (check-true (equal? (apply op args) (apply op args))))
   
(define value-tests
  (test-suite
   "Tests for rosette/base/term.rkt"
   #:before (lambda () (printf "Testing rosette/base/term.rkt\n"))

   (check-false (term<? x x))
   (check-false (term<? a a))
   (check-false (term<? (&& a b) (&& b a)))
   (check-false (term<? (@+ x y z) (@+ x y z))) 
   
   (check-ordered b a)
   (check-ordered x a)
   (check-ordered x y)
   (check-ordered (! b) (! a))
   (check-ordered (&& b a) (&& a c))
   (check-ordered x (@* x y))
   (check-ordered (@/ x y) (@- x y))
   (check-ordered (@expt x y) (@+ x y z))
   (check-ordered a (|| a b))

   (check-cached && a b)
   (check-cached || a b)
   (check-cached ! a)
   (check-cached @+ x y)
   (check-cached @- x y)
   (check-cached @* x y)
   (check-cached @/ x y)
   (check-cached @expt x y)
   (check-cached @= x y)
   (check-cached @< x y)))

(time (run-tests value-tests))
