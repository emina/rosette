#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term
         (only-in rosette/base/core/bool
                  @boolean?
                  [@&& &&] [@|| ||] [@! !] [@=> =>] [@<=> <=>])
         (only-in rosette/base/form/define define-symbolic)
         "common.rkt" "solver.rkt")

(define-symbolic a b c d @boolean?)


(define-syntax (unsimplified stx)
  (syntax-case stx ()
    [(_ (op arg ...)) #'(expression op (unsimplified arg) ...)]
    [(_ v)            #'v]))


(define-syntax-rule (check-rewrite (op arg ...) expected)
  (let ([actual (op arg ...)])
    (check-equal? actual expected)
    (check-unsat (solve (expression ! (expression <=> actual (unsimplified (op arg ...))))))))


(define (check-connective-pe op co id ref)
  (check-nary op id a b c)
  
  (check-rewrite (op #f #f) (ref #f #f))
  (check-rewrite (op #f #t) (ref #f #t))
  (check-rewrite (op #t #f) (ref #t #f))
  (check-rewrite (op #t #t) (ref #t #t))

  (check-rewrite (op a id) a)
  (check-rewrite (op a b id) (op a b))
  (check-rewrite (op a (! id)) (! id))
  (check-rewrite (op a b (! id)) (! id))

  (check-rewrite (op a a) a)
  (check-rewrite (op a (! a)) (! id))
  (check-rewrite (op (! a) a) (! id))

  (check-rewrite (op (co a b c) b) b)
  (check-rewrite (op b (co a b c)) b)

  (check-rewrite (op (op a b c) b) (op a b c))
  (check-rewrite (op b (op a b c)) (op a b c))

  (check-rewrite (op (op a (! b) c) b) (! id))
  (check-rewrite (op b (op a (! b) c)) (! id))

  (check-rewrite (op (! (co a b c)) b) (! id))
  (check-rewrite (op b (! (co a b c))) (! id))

  (check-rewrite (op (op a b c) (! b)) (! id))
  (check-rewrite (op (! b) (op a b c)) (! id))

  (check-rewrite (op (op a b) (op a b c)) (op a b c))
  (check-rewrite (op (op a b c) (op a b)) (op a b c))

  (check-rewrite (op (op a (! b)) (op a b c)) (! id))
  (check-rewrite (op (op a b c) (op a (! b))) (! id))
  (check-rewrite (op (op a (! b) c) (op a b c)) (! id))
  (check-rewrite (op (op a b c) (op a (! b) c)) (! id))

  (check-rewrite (op (co a b) (co a b c)) (co a b))
  (check-rewrite (op (co a b c) (co a b)) (co a b))
  (check-rewrite (op (co a b c) (co a b c)) (co a b c))

  (check-rewrite (op (op a b) (co c b d)) (op a b))
  (check-rewrite (op (co c b d) (op a b)) (op a b))
  
  (check-equal? (op c b a a a a b a c c ) (op a b c))
  (check-equal? (op c b a a a a b a (! c) c ) (! id))
  (check-equal? (op c (co c a) (co c b) a (co a b)) (op a c))
  (check-equal? (op (co a b) (co a b c)) (co a b))
  (check-equal? (op (co a b c) (co a b)) (co a b))
  (check-equal? (op (co a b) (co a b c) a) a)
  (check-equal? (op (co a b c) (co a b) a) a))

(define (check-not)
  (check-rewrite (! #t) #f)
  (check-rewrite (! #f) #t)
  (check-rewrite (! (! a)) a))

(define (check-iff)
  (check-rewrite (<=> #t #t) #t)
  (check-rewrite (<=> #f #f) #t)
  (check-rewrite (<=> #t #f) #f)
  (check-rewrite (<=> #f #t) #f)
   
  (check-rewrite (<=> a a) #t)
  (check-rewrite (<=> a (! a)) #f)
  (check-rewrite (<=> (! a) a) #f)
  (check-rewrite (<=> a #t) a)
  (check-rewrite (<=> #t a) a)
  (check-rewrite (<=> a #f) (! a))
  (check-rewrite (<=> #f a) (! a))
  (check-rewrite (<=> a b) (<=> b a))
  (check-false (boolean? (<=> a b))))

(define (check-if)
  (check-rewrite (=> #f #f) #t)
  (check-rewrite (=> #f #t) #t)
  (check-rewrite (=> #t #f) #f)
  (check-rewrite (=> #t #t) #t)
  
  (check-rewrite (=> a a) #t)
  (check-rewrite (=> #f a) #t)
  (check-rewrite (=> a #t) #t)
  (check-rewrite (=> #t a) a)
  (check-rewrite (=> a #f) (! a))
  (check-rewrite (=> (! a) a) a)
  (check-rewrite (=> a (! a)) (! a))

  (check-rewrite (=> a (|| a b c)) #t)
  (check-rewrite (=> a (|| (! a) b)) (=> a b))
  (check-rewrite (=> d (|| b (! d))) (=> d b))
  (check-rewrite (=> a (&& a b)) (=> a b))
  (check-rewrite (=> d (&& b d)) (=> d b))
  (check-rewrite (=> a (&& (|| a c) b)) (=> a b))
  (check-rewrite (=> a (&& b (|| a c))) (=> a b))
  (check-rewrite (=> a (<=> a b)) (=> a b))
  (check-rewrite (=> d (<=> b d)) (=> d b))
  (check-rewrite (=> a c) (|| (! a) c)))

(define bool-tests
  (test-suite+ 
   "Tests for rosette/base/bool.rkt" 
   #:features '(qf_uf)
   
   (check-not)
   (check-connective-pe && || #t (lambda xs (andmap identity xs)))
   (check-connective-pe || && #f (lambda xs (ormap identity xs)))
   (check-iff)
   (check-if)))

(module+ test
  (time (run-tests bool-tests)))
