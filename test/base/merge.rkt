#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/real
         rosette/base/core/procedure
         rosette/base/core/polymorphic
         rosette/base/core/merge
         (only-in rosette/base/adt/list @list?)         
         rosette/base/struct/struct
         (only-in rosette/base/form/define define-symbolic)
         "common.rkt" (only-in "vc.rkt" check-vc-eqv))

(define-symbolic x y z w @integer?)

(define-symbolic a b c @boolean?)

(define (basic-merge-tests)
  (check-equal? (merge #t x y) x)
  (check-equal? (merge #f x y) y)
  (check-equal? (merge a x x) x)
  (check-equal? (merge (! a) x x) x)
  
  (check-equal? (merge a b c) (|| (&& a b) (&& (! a) c)))
  
  (check-equal? (merge a (merge a x y) z) (merge a x z))
  (check-equal? (merge a (merge (! a) x y) z) (merge a y z))
  (check-equal? (merge a z (merge a x y)) (merge a z y))
  (check-equal? (merge a z (merge (! a) x y)) (merge a z x))
  (check-equal? (merge a (merge a x y) (merge a w z)) (merge a x z))
  (check-equal? (merge a (merge a x y) (merge (! a) w z)) (merge a x w))
  (check-equal? (merge a (merge (! a) x y) (merge a w z)) (merge a y z))
  (check-equal? (merge a (merge (! a) x y) (merge (! a) w z)) (merge a y w))
  
  (check-equal? (merge a (@+ x y) 0) (merge a (@+ y x) 0))
  
  (check-equal? (merge* (cons a b) (cons #f x) (cons b y) (cons #t z) (cons c w)) z)
  (check-union? (merge* (cons a b) (cons #f x) (cons b y) (cons c w))
              { [a b] [(|| b c) (ite* (cons c w) (cons b y))] }))

(define (list-merge-tests)
  (check-equal? (merge a (list) (list)) (list)) 
  (check-equal? (merge a (list b) (list c)) (list (|| (&& c (! a)) (&& a b))))
  (check-union? (merge a (list b) (merge a (list x y) (list)))
              { [a (list b)] [(! a) (list)]})
  (check-union? (merge a (list b) (merge (! a) (list x y) (list)))
              { [a (list b)] [(! a) (list x y)]})
  (check-union? (merge a (merge b (list b) (list c)) (merge c (list x y) (list)))
              { [a (list (|| b (&& c (! b))))] 
                [(&& c (! a)) (list x y)] 
                [(&& (! a) (! c)) (list)]})
  (check-union? (merge a (merge b (list b) z) (merge c (list x y) z))
              { [(&& a b) (list b)] [(|| (&& a (! b)) (&& (! a) (! c))) z] 
                [(&& (! a) c) (list x y)]}))

(struct S (x)
  #:property prop:procedure
  (lambda (self) (S-x self)))

(struct P S ())
(struct Q ())


(define (procedure-merge-tests)
  (check-equal? (merge a identity identity) identity)
  (check-equal? (@procedure? (merge a identity 2)) a)
  
  (define s (S 1))
  (define p (P 2))
  (define q (Q))
  
  (check-true (@procedure? s))
  (check-true (subtype? (type-of s) @procedure?))
  (check-true (subtype? (type-of p) @procedure?))
  (check-false (subtype? (type-of q) @procedure?))
  
  (clear-vc!)
  (define s* (merge a s *))
  (check-equal? (s*) 1)
  (check-pred vc-true? (vc))
  (check-equal? (s* 3 2) 6)
  (check-vc-eqv #t (! a))
  (clear-vc!) 
  
  (define (kw #:kw y) (- y))
  (define f (merge b + 'f))
  (define g (merge c s* f))
  (check-equal? (g) (ite* (cons (&& b (! c)) 0) (cons (|| (&& c (! a)) (&& a c)) 1)))
  (check-vc-eqv #t (|| c (&& b (! c))))
  (clear-vc!)

  (define h (merge c kw f))
  (check-equal? (h #:kw 3) -3)
  (check-vc-eqv #t c)
  (clear-vc!) )
 
  
  
(define merge-tests
  (test-suite+ 
   "Tests for rosette/base/merge.rkt"
   
   (basic-merge-tests)
   (list-merge-tests)
   (procedure-merge-tests)))

(module+ test
  (time (run-tests merge-tests)))

