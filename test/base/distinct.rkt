#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic a b c boolean?)
(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)
(define-symbolic n1 n2 n3 (bitvector 4))
  
(define (all-different . xs)
  (match xs
    [(list) #t]
    [(list _) #t]
    [(list x y) (not (equal? x y))]
    [(list x ys ...)
     (match (apply && (for/list ([y ys]) (not (equal? x y))))
       [#f #f]
       [t (&& t (apply all-different ys))])]))

(define (check-real)
  (check-equal? (distinct?) #t)
  (check-equal? (distinct? 1) #t)
  (check-equal? (distinct? 3.4) #t)
  (check-equal? (distinct? 1 2) #t)
  (check-equal? (distinct? 2.3 1) #t)
  (check-equal? (distinct? 1 1) #f)
  (check-equal? (distinct? 2.3 2.3) #f)
  (check-equal? (distinct? xi yi) (not (equal? xi yi)))
  (check-equal? (distinct? xr yr) (not (equal? xr yr)))
  (check-equal? (distinct? xi yr) (not (equal? xi yr)))
  (check-equal? (distinct? xi yr xi zr) #f)
  (check-equal? (distinct? 3 xi 1 yr 1 zr) #f)
  (check-unsat (verify (assert (equal? (distinct? xi yi zi) (all-different xi yi zi)))))
  (check-unsat (verify (assert (equal? (distinct? xi 4 yi 5 zi) (all-different 4 xi yi zi 5)))))
  (check-unsat (verify (assert (equal? (distinct? xi 4.4 yi 5 zi) (all-different 4.4 xi yi zi 5)))))
  (check-unsat (verify (assert (equal? (distinct? xr yr zr) (all-different xr yr zr)))))
  (check-unsat (verify (assert (equal? (distinct? xr 4 yr 5 zr) (all-different 4 xr yr zr 5.0)))))
  (check-unsat (verify (assert (equal? (distinct? xi 4.4 yr 5 zi) (all-different 4.4 xi yr zi 5)))))
  )
 

(define tests:bool
  (test-suite+
   "Tests for distinct? over booleans."
   (check-equal? (distinct?) #t)
   (check-equal? (distinct? #t) #t)
   (check-equal? (distinct? #f) #t)
   (check-equal? (distinct? #t #f) #t)
   (check-equal? (distinct? #f #t) #t)
   (check-equal? (distinct? #t #t) #f)
   (check-equal? (distinct? #f #f) #f)
   (check-equal? (distinct? a #t) (! a))
   (check-equal? (distinct? a #f) a)
   (check-equal? (distinct? a a) #f)
   (check-equal? (distinct? a b) (not (equal? a b)))
   (check-equal? (distinct? a b c) #f)))

(define tests:real
  (test-suite+
   "Tests for distinct? over infinite precision integers and reals."
   (current-bitwidth #f)
   (check-real)
   ))

(define tests:real-finitized
  (test-suite+
   "Tests for distinct? over finite precision integers and reals."
   (current-bitwidth 5)
   (check-real)
   ))

(time (run-tests tests:bool))
(time (run-tests tests:real))
(time (run-tests tests:real-finitized))