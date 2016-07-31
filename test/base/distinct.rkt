#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic a b c boolean?)
(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)
(define-symbolic xb yb zb (bitvector 4))
  
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
  (check-equal? (distinct? (+ xi 4.4 yr) 5 zi (+ 4.4 yr xi )) #f)
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

(define tests:bitvector
  (test-suite+
   "Tests for distinct? bitvectors."
   (current-bitwidth #f)
   (check-equal? (distinct? (bv 1 1)) #t)
   (check-equal? (distinct? (bv 5 5)) #t)
   (check-equal? (distinct? (bv 5 5) (bv 1 5)) #t)
   (check-equal? (distinct? (bv 3 1) (bv 1 1)) #f)
   (check-equal? (distinct? xb yb) (not (equal? xb yb)))
   (check-equal? (distinct? xb yb xb zb) #f)
   (check-equal? (distinct? (bv 3 4) xb (bv 1 4) yb (bv 1 4) zb) #f)
   (check-unsat (verify (assert (equal? (distinct? xb yb zb) (all-different xb yb zb)))))
   (check-unsat (verify (assert (equal? (distinct? xb (bv 4 4) yb (bv 5 4) zb)
                                        (all-different  xb (bv 4 4) yb (bv 5 4) zb)))))
   (check-equal? (distinct? (bvadd xb (bv 4 4) yb) (bv 5 4) zb (bvadd xb (bv 4 4) yb)) #f)))

(define tests:mixed
  (test-suite+
   "Tests for distinct? non-primitives and mixed values."
    (current-bitwidth #f)
    (define-symbolic f g (~> integer? real?))
    (check-equal? (distinct? (list)) #t)
    (check-equal? (distinct? (list 1)) #t)
    (check-equal? (distinct? (list 1) (list 3 4)) #t)
    (check-equal? (distinct? (list 1) (list 1)) #f)
    (check-equal? (distinct? (list a b xi yr) (list #t #f 1 2))
                  (not (equal? (list a b xi yr) (list #t #f 1 2))))
    (check-unsat (verify (assert (equal? (distinct? f g zb) (all-different zb g f)))))
    (check-unsat (verify (assert (equal? (distinct? xr 1 f g zb yi) (all-different xr 1 zb g f yi)))))
    ))

(define tests:regression
  (test-suite+
   "Regression tests for distinct?"
   (define (test n)
     (current-bitwidth #f)
     (define-symbolic f (~> integer? integer?))
     (let ([xs (build-list n identity)])
       (solve (assert (apply distinct? (map f xs))))))
   (check-sat (test 1))
   (check-sat (test 32))
   (check-sat (test 33))))

(time (run-tests tests:bool))
(time (run-tests tests:real))
(time (run-tests tests:real-finitized))
(time (run-tests tests:bitvector))
(time (run-tests tests:mixed))
(time (run-tests tests:regression))