#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/real
         (only-in rosette/base/form/define define-symbolic))

(define-symbolic x @integer?)
(define-symbolic y @integer?)
(define-symbolic z @integer?)

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

(define (f type)
  (define-symbolic x type)
  x)

(define (check-ordered v1 v2)
  (check-true (and (or (term<? v1 v2) (term<? v2 v1)) 
                   (not (and (term<? v1 v2) (term<? v2 v1))))))

(define (check-cached op . args)
  (check-true (equal? (apply op args) (apply op args))))
   
(define value-tests
  (test-suite+
   "Tests for rosette/base/term.rkt"

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
   (check-ordered (@remainder x y) (@+ x y z))
   (check-ordered a (|| a b))

   (check-cached && a b)
   (check-cached || a b)
   (check-cached ! a)
   (check-cached @+ x y)
   (check-cached @- x y)
   (check-cached @* x y)
   (check-cached @/ x y)
   (check-cached @remainder x y)
   (check-cached @= x y)
   (check-cached @< x y)

   (f @integer?)
   (check-exn #px"type should remain unchanged" (lambda () (f @boolean?)))))

(define clear-terms!+gc-terms!-tests
  (test-suite+
   "Tests for clear-terms! and gc-terms!"

   (with-terms '()
     (let ()
       (define-symbolic x y z @integer?)
       (define a (@+ x 1))
       (define b (@+ y 2))
       (define c (@+ z 3))
       (check-equal? (length (terms)) 6)

       ;; this should evict z and c
       (clear-terms! (list z))

       (check-equal? (length (terms)) 4)

       ;; this doesn't affect strongly-held values
       (set! b #f)

       (check-equal? (length (terms)) 4)

       (gc-terms!) ; change the representation
       (collect-garbage)

       (check-equal? (length (terms)) 3)

       (clear-terms! (list x))
       (collect-garbage)

       (check-equal? (length (terms)) 1)

       ;; this is a dummy check to reference a, b, and c so that
       ;; they are not garbage-collected earlier
       (check-equal? (length (list a b c)) 3)))))

(module+ test
  (time (run-tests value-tests))
  (time (run-tests clear-terms!+gc-terms!-tests)))
