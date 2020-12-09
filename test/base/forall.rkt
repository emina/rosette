#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/merge)

(define-syntax (check-for/all stx)
  (syntax-case stx ()
    [(_ (head ([v val mod ...]) expr ...) expected-values)
     (free-identifier=? #'head #'for/all)
     #'(check-for/all #:assume #t (head ([v val mod ...]) expr ...)
                      expected-values)]
    [(_ #:assume pre (head ([v val mod ...]) expr ...) expected-values)
     (free-identifier=? #'head #'for/all)
     #'(let ([actual-values (mutable-set)]
             [proc (λ (v) expr ...)])
         (check-unsat
          (result-value
           (with-vc
               (begin
                 pre
                 (verify
                  (assert
                   (equal? (head ([v val mod ...])
                                 (begin (set-add! actual-values v) (proc v)))
                           (proc val))))))))
         (check set=? actual-values expected-values))]))

(define-symbolic a b c d e boolean?)
(define-symbolic f i j k integer?)
(define-symbolic p q r s real?)
(define-symbolic u w x y (bitvector 2))

(define (check-basic-for/all)
  (check-for/all ; concrete value
   (for/all ([v 3])
     (equal? v 3.0))
   (set 3))
  (check-for/all ; union
   (for/all ([v (if a f (if b p 'p))])
     (equal? v 1.0))
   (set f p 'p))
  (check-for/all ; opaque ite
   (for/all ([v (if a f i)])
     (equal? v 1.0))
   (set (if a f i)))
  (check-for/all ; union over opaque ite
   (for/all ([v (if a f (if b p (if c u w)))])
     (equal? v 1.0))
   (set f p (if c u w)))
  (check-for/all ; union with multiple expressions in the body
   (for/all ([v (if a f (if b p 'p))])
     (void)
     (equal? v 1.0))
   (set f p 'p))
  )

(define (check-exhaustive-for/all)
  (check-for/all ; concrete value
   (for/all ([v 3 #:exhaustive])
     (equal? v 3.0))
   (set 3))
  (check-for/all ; union
   (for/all ([v (if a f (if b p 'p)) #:exhaustive])
     (equal? v 1.0))
   (set f p 'p))
  (check-for/all ; transparent ite
   (for/all ([v (if a f i) #:exhaustive])
     (equal? v 1.0))
   (set f i))
  (check-for/all ; transparent ite
   (for/all ([v (if a f (if b i (if c j k))) #:exhaustive])
     (equal? v 1))
   (set f i j k))
  (check-for/all ; union over transparent ite
   (for/all ([v (if a f (if b p (if c u w))) #:exhaustive])
     (equal? v 1.0))
   (set f p u w))
  (check-for/all ; union over transparent ite*
   (for/all ([v (if a f (if b i (if c p k))) #:exhaustive])
     (equal? v 1))
   (set f i p k))
  )

(define (check-concretized-for/all)
  (check-for/all
   (for/all ([v (if a 1 (if b 2 3)) (list 1 2 3)])
     (<= 0 v 4))
   (set 1 2 3))
  (check-for/all
   #:assume (assert (<= 1 i 3))
   (for/all ([v i (list 1 2 3)])
     (add1 v))
   (set 1 2 3))
  (check-for/all
   (for/all ([v 2 '(1 2)]) v)
   (set 2))
  )

(define tests:basic
  (test-suite+
   "Check the (for/all ([v val]) expr ...+) form."
   (current-bitwidth #f)
   (check-basic-for/all)))

(define tests:exhaustive
  (test-suite+
   "Check the (for/all ([v val #:exhaustive]) expr ...+) form."
   (current-bitwidth #f)
   (check-exhaustive-for/all)))

(define tests:concretized
  (test-suite+
   "Check the (for/all ([v val concretized]) expr ...+) form."
   (current-bitwidth #f)
   (check-concretized-for/all)))

(module+ test
  (time (run-tests tests:basic))
  (time (run-tests tests:exhaustive))
  (time (run-tests tests:concretized)))
