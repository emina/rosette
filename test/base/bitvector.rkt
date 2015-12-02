#lang racket

(require rackunit rackunit/text-ui racket/generator
         rosette/solver/solution 
         rosette/lib/util/roseunit rosette/solver/smt/z3 
         racket/fixnum 
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/bitvector
         rosette/base/core/polymorphic
         (only-in rosette/base/core/equality @equal?)
         (only-in rosette/base/form/define define-symbolic)
         "exprs.rkt" "common.rkt")

(define solver (new z3%))

(current-bitwidth 4)
(define BV (bitvector 4))
(define-symbolic x y z BV)
(define-symbolic a b @boolean?)

(define minval (- (expt 2 (sub1 (bitvector-size BV)))))
(define maxval+1 (expt 2 (sub1 (bitvector-size BV)))) 
(define maxval (sub1 maxval+1))

(define (solve  . asserts)
  (send/apply solver assert asserts)
  (begin0
    (send solver solve)
    (send solver clear)))

(define (check-semantics op)
  (case (procedure-arity op)
    [(1) 
     (for ([i (in-range minval maxval+1)])
       (define actual (op (bv i)))
       (define expected 
         ((solve (@bveq (bv i) x)
                 (@bveq y (op x))) y))
       (check-equal? actual expected))]
    [else
     (for* ([i (in-range minval maxval+1)]
            [j (in-range minval maxval+1)])
       (define actual (op (bv i) (bv j)))
       ;(printf "(~a ~a ~a) = ~a\n" op (bv i) (bv j) actual)
       (define expected 
         ((solve (@bveq (bv i) x)
                 (@bveq (bv j) y)
                 (@bveq z (op x y))) z))
       (check-equal? actual expected))]))

(define (check-cmp-semantics op)
  (for* ([i (in-range minval maxval+1)]
            [j (in-range minval maxval+1)])
       (define actual (op (bv i) (bv j)))
       (define expected 
         ((solve (@bveq (bv i) x)
                 (@bveq (bv j) y)
                 (@equal? a (op x y))) a))
       (check-equal? actual expected)))

(define (check-pe ops [consts '()])
  (define es (test-exprs 2 ops (list* x y z consts)))
  ;(printf "exprs: ~a\n" (length es))
  (for ([e es])
   ;(printf "~e = ~e\n" e (reduce e))
    (check-pred unsat? (solve (! (@bveq e (reduce e)))))))

(define-syntax-rule (check-valid? (op e ...) expected)
  (let ([actual (op e ...)])
    (check-equal? actual expected)
    (check-pred unsat? (solve (! (@equal? (expression op e ...) expected))))))

(define (check-bveq-simplifications)
  (check-valid? (@bveq (ite b (bv 2) (bv 0)) (bv 2)) b)
  (check-valid? (@bveq (ite b (bv 2) (bv 0)) (bv 0)) (! b))
  (check-valid? (@bveq (ite b (bv 2) (bv 0)) (bv 1)) #f)
  (check-valid? (@bveq (bv 2) (ite b (bv 2) (bv 0))) b)
  (check-valid? (@bveq (bv 0) (ite b (bv 2) (bv 0))) (! b))
  (check-valid? (@bveq (bv 1) (ite b (bv 2) (bv 0))) #f)
  (check-valid? (@bveq (ite a (bv 2) (bv 2)) (ite b (bv 2) (bv 2))) #t)
  (check-valid? (@bveq (ite a (bv 2) (bv 3)) (ite b (bv 4) (bv 5))) #f)
  (check-valid? (@bveq (ite a (bv 2) (bv 3)) (ite b (bv 2) (bv 5))) (&& a b))
  (check-valid? (@bveq (ite a (bv 2) (bv 3)) (ite b (bv 5) (bv 2))) (&& a (! b)))
  (check-valid? (@bveq (ite a (bv 2) (bv 3)) (ite b (bv 3) (bv 5))) (&& (! a) b))
  (check-valid? (@bveq (ite a (bv 2) (bv 3)) (ite b (bv 5) (bv 3))) (&& (! a) (! b))))
                
(define (check-bvcmp-simplifications l* g*)
  (check-equal? (g* x y) (l* y x))
  (check-valid? (l* (ite b (bv 2) (bv 0)) (bv 1)) (! b))
  (check-valid? (l* (ite b (bv 1) (bv 3)) (bv 2)) b)
  (check-valid? (l* (ite b (bv 2) (bv 1)) (bv 3)) #t)
  (check-valid? (l* (ite b (bv 2) (bv 1)) (bv 0)) #f)
  (check-valid? (l* (bv 1) (ite b (bv 2) (bv 0))) b)
  (check-valid? (l* (bv 1) (ite b (bv 0) (bv 3))) (! b))
  (check-valid? (l* (bv 3) (ite b (bv 2) (bv 1))) #f)
  (check-valid? (l* (bv 0) (ite b (bv 2) (bv 1))) #t)
  (check-valid? (l* (ite a (bv 2) (bv 1)) (ite b (bv 3) (bv 4))) #t)
  (check-valid? (l* (ite b (bv 3) (bv 4)) (ite a (bv 2) (bv 1))) #f)
  (check-valid? (l* (ite a (bv 3) (bv 1)) (ite b (bv 0) (bv 2))) (&& (! a) (! b)))
  (check-valid? (l* (ite a (bv 1) (bv 3)) (ite b (bv 2) (bv 0))) (&& a b))
  (check-valid? (l* (ite a (bv 1) (bv 3)) (ite b (bv 0) (bv 2))) (&& a (! b)))
  (check-valid? (l* (ite a (bv 3) (bv 1)) (ite b (bv 2) (bv 0))) (&& (! a) b)))

(define (check-bvslt-simplifications)
  (check-bvcmp-simplifications @bvslt @bvsgt)
  (check-valid? (@bvslt x (bv maxval)) (! (@bveq x (bv maxval))))
  (check-valid? (@bvslt (bv maxval) x) #f)
  (check-valid? (@bvslt x (bv minval)) #f)
  (check-valid? (@bvslt (bv minval) x) (! (@bveq x (bv minval)))))

(define (check-bvsle-simplifications)
  (check-bvcmp-simplifications @bvsle @bvsge)
  (check-valid? (@bvsle x (bv maxval)) #t)
  (check-valid? (@bvsle (bv maxval) x) (@bveq x (bv maxval)))
  (check-valid? (@bvsle x (bv minval)) (@bveq x (bv minval)))
  (check-valid? (@bvsle (bv minval) x) #t))

(define (check-bvult-simplifications)
  (check-bvcmp-simplifications @bvult @bvugt)
  (check-valid? (@bvult x (bv -1)) (! (@bveq x (bv -1))))
  (check-valid? (@bvult (bv -1) x) #f)
  (check-valid? (@bvult x (bv 0)) #f)
  (check-valid? (@bvult (bv 0) x) (! (@bveq x (bv 0)))))

(define (check-bvule-simplifications)
  (check-bvcmp-simplifications @bvule @bvuge)
  (check-valid? (@bvule x (bv -1)) #t)
  (check-valid? (@bvule (bv -1) x) (@bveq x (bv -1)))
  (check-valid? (@bvule x (bv 0)) (@bveq x (bv 0)))
  (check-valid? (@bvule (bv 0) x) #t))

(define (check-bitwise-simplifications op co id)
  (check-nary op id x y z)
  (check-valid? (op (bv 1) x) (op x (bv 1)))
  (check-valid? (op x (@bvnot x) ) (@bvnot id))
  (check-valid? (op x (@bvnot id)) (@bvnot id))
  (check-valid? (op x (@bvnot id) y z id) (@bvnot id))
  (check-valid? (op (co x y) (@bvnot (co x y))) (@bvnot id))
  (check-valid? (op (op x y) (@bvnot (op x y))) (@bvnot id))
  (check-valid? (op x (@bvnot id)) (@bvnot id))
  (check-valid? (@bvnot (@bvnot (op x y))) (op x y))
  (check-valid? (op z y x x x x y x z z ) (op x y z))
  (check-valid? (op (@bvnot (bv minval)) z y x x x x (bv minval)  y x z z )  (@bvnot id))
  (check-valid? (op z y x x x x (bv minval)  y x z z (bv 2) )  (op (op (bv minval) (bv 2)) x y z))
  (check-valid? (op z y x x x x y x (@bvnot z) z ) (@bvnot id))
  (check-valid? (op z (co z x)  (co z y) x (co x y)) (op x z))
  (check-valid? (op (co x y) (co x y z)) (co x y))
  (check-valid? (op (co x y z) (co x y)) (co x y))
  (check-valid? (op (co x y) (co x y z) x)  x) 
  (check-valid? (op (co x y z) (co x y) x)  x)
  (check-valid? (op (op x (bv minval) z) (@bvnot (bv minval)))  (@bvnot id))
  (check-valid? (op (op x (bv minval) z) (op y (@bvnot (bv minval))))  (@bvnot id)))

(define (check-shift-simplifications op)
  (check-valid? (op x (bv 0)) x)
  (check-valid? (op (bv 0) x) (bv 0))
  (check-valid? (op x (bv 4)) (bv 0))
  (check-valid? (op x (bv 5)) (bv 0))
  (check-valid? (op x (bv -1)) (bv 0)))

(define (check-bvashr-simplifications)
  (check-valid? (@bvashr x (bv 0)) x)
  (check-valid? (@bvashr (bv 0) x) (bv 0))
  (check-valid? (@bvashr (bv -1) x) (bv -1))
  (check-valid? (@bvashr x (bv 4)) (ite (@bveq (bv 0) (@bvand x (bv minval))) (bv 0) (bv -1)))
  (check-valid? (@bvashr x (bv 5)) (ite (@bveq (bv 0) (@bvand x (bv minval))) (bv 0) (bv -1)))
  (check-valid? (@bvashr x (bv -1)) (ite (@bveq (bv 0) (@bvand x (bv minval))) (bv 0) (bv -1))))

(define (check-bvadd-simplifications)
  (check-nary @bvadd (bv 0) x y z)
  (check-valid? (@bvadd (bv 1) x) (@bvadd x (bv 1)))
  (check-valid? (@bvadd x (@bvneg x)) (bv 0))
  (check-valid? (@bvadd (@bvneg x) x) (bv 0))
  (check-valid? (@bvadd (@bvneg (@bvadd x y)) x) (@bvneg y))
  (check-valid? (@bvadd (@bvneg (@bvadd x y)) y) (@bvneg x))
  (check-valid? (@bvadd (@bvadd (@bvneg x) y) x) y)
  (check-valid? (@bvadd (@bvadd x (@bvneg y)) y) x)
  (check-valid? (@bvadd (@bvadd (bv 5) y) (bv -5)) y)
  (check-valid? (@bvadd (@bvadd x y) (@bvneg x)) y)
  (check-valid? (@bvadd (@bvadd x y) (@bvneg y)) x)
  (check-valid? (@bvadd (@bvadd x y) (@bvadd (@bvneg x) (@bvneg y) z)) z)
  (check-valid? (@bvadd (@bvadd x y z) (@bvadd (@bvneg x) (@bvneg y) (@bvneg z))) (bv 0))
  (check-valid? (@bvadd (@bvadd y z) (@bvadd (bv 1) (@bvneg y) (@bvneg z))) (bv 1))
  (check-valid? (@bvadd (@bvadd (bv 1) y z) (@bvadd (bv -1) (@bvneg y) (@bvneg z))) (bv 0))
  (check-valid? (@bvadd (@bvadd (bv 1) y z) (@bvadd (bv -1) (@bvneg z))) y)
  (check-valid? (@bvadd x y z (@bvadd (@bvneg x) (@bvneg y))) z)
  (check-valid? (@bvadd x y z (@bvadd (@bvneg x) (@bvneg y)) (@bvneg z)) (bv 0))
  (check-valid? (@bvadd x (bv 0) y (bv 1) z (bv 5)) (@bvadd (bv 6) x y z))
  (check-valid? (@bvadd (@bvmul y x) (@bvmul x (@bvneg y))) (bv 0))
  (check-valid? (@bvadd (@bvmul x (@bvneg y)) (@bvmul y x)) (bv 0))
  (check-valid? (@bvadd (@bvmul (bv 3) x) (@bvmul x (bv -3))) (bv 0))
  (check-valid? (@bvadd (@bvmul (bv 3) x) (@bvmul x (bv 2))) (@bvmul (bv 5) x)))

(define (check-bvmul-simplifications)
  (check-nary @bvmul (bv 1) x y z)
  (check-valid? (@bvmul (bv 3) x) (@bvmul x (bv 3)))
  (check-valid? (@bvmul (bv 0) x) (bv 0))
  (check-valid? (@bvmul x (bv 0)) (bv 0))
  (check-valid? (@bvmul x (bv -1)) (@bvneg x))
  (check-valid? (@bvmul (bv -1) x) (@bvneg x))
  (check-valid? (@bvmul (@bvmul x (bv 2)) (bv 3)) (@bvmul (bv 6) x)))

(define (check-bvudiv-simplifications)
  (check-valid? (@bvudiv (bv 3) (bv 0)) (bv -1))
  (check-valid? (@bvudiv x (bv 0)) (bv -1))
  (check-valid? (@bvudiv (bv 3) (bv 1)) (bv 3))
  (check-valid? (@bvudiv x (bv 1)) x)
  (check-valid? (@bvudiv x (bv -1)) (ite (@bveq x (bv -1)) (bv 1) (bv 0)))
  (check-valid? (@bvudiv (bv 0) x) (ite (@bveq x (bv 0)) (bv -1) (bv 0)))
  (check-valid? (@bvudiv x x) (ite (@bveq x (bv 0)) (bv -1) (bv 1)))
  (check-valid? (@bvudiv (ite b (bv 6) (bv 8)) (bv 2)) (ite b (bv 3) (bv 4)))
  (check-valid? (@bvudiv (bv 6) (ite b (bv 2) (bv 3))) (ite b (bv 3) (bv 2))))

(define (check-bvsdiv-simplifications)
  (check-valid? (@bvsdiv (bv 3) (bv 0)) (bv -1))
  (check-valid? (@bvsdiv (bv -3) (bv 0)) (bv 1))
  (check-valid? (@bvsdiv x (bv 0)) (ite (@bvslt x (bv 0)) (bv 1) (bv -1)))
  (check-valid? (@bvsdiv (bv 3) (bv 1)) (bv 3))
  (check-valid? (@bvsdiv x (bv 1)) x)
  (check-valid? (@bvsdiv x (bv -1)) (@bvneg x))
  (check-valid? (@bvsdiv (bv 0) x) (ite (@bveq x (bv 0)) (bv -1) (bv 0)))
  (check-valid? (@bvsdiv x (bv minval)) (ite (@bveq x (bv minval)) (bv 1) (bv 0)))
  (check-valid? (@bvsdiv x x) (ite (@bveq x (bv 0)) (bv -1) (bv 1)))
  (check-valid? (@bvsdiv x (@bvneg x)) (ite (@bveq x (bv minval)) (bv 1) (bv -1)))
  (check-valid? (@bvsdiv (@bvneg x) x) (ite (@bveq x (bv minval)) (bv 1) (bv -1)))
  (check-valid? (@bvsdiv (ite b (bv -6) (bv 4)) (bv 2)) (ite b (bv -3) (bv 2)))
  (check-valid? (@bvsdiv (bv 6) (ite b (bv 2) (bv 3))) (ite b (bv 3) (bv 2))))

(define (check-bvurem-simplifications)
  (check-valid? (@bvurem (bv 3) (bv 0)) (bv 3))
  (check-valid? (@bvurem (bv 0) (bv 0)) (bv 0))
  (check-valid? (@bvurem (bv -3) (bv 0)) (bv -3))
  (check-valid? (@bvurem x (bv 0)) x)
  (check-valid? (@bvurem (@bvneg x) (bv 0))  (@bvneg x))
  (check-valid? (@bvurem (bv 0) (bv 3)) (bv 0))
  (check-valid? (@bvurem (bv 0) (bv 0)) (bv 0))
  (check-valid? (@bvurem (bv 0) (bv -3)) (bv 0))
  (check-valid? (@bvurem (bv 0) x) (bv 0))
  (check-valid? (@bvurem (bv 0) (@bvneg x)) (bv 0))
  (check-valid? (@bvurem (bv 3) (bv 1)) (bv 0))
  (check-valid? (@bvurem (bv 0) (bv 1)) (bv 0))
  (check-valid? (@bvurem (bv -3) (bv 1)) (bv 0))
  (check-valid? (@bvurem x (bv 1)) (bv 0))
  (check-valid? (@bvurem (@bvneg x) (bv 1)) (bv 0))
  (check-valid? (@bvurem x (bv -1)) (ite (@bveq x (bv -1)) (bv 0) x))
  (check-valid? (@bvurem x x) (bv 0))
  (check-valid? (@bvurem (@bvneg x) (@bvneg x)) (bv 0))
  (check-valid? (@bvurem (ite b (bv 6) (bv 4)) (bv 3)) (ite b (bv 0) (bv 1)))
  (check-valid? (@bvurem (bv 6) (ite b (bv 4) (bv 5))) (ite b (bv 2) (bv 1))))
  
(define tests:bv
  (test-suite+
   "Tests for bv in rosette/base/bitvector.rkt"
   (check-equal? (bv minval) (bv maxval+1))
   (check-equal? (bv (sub1 minval)) (bv (sub1 maxval+1)))))

(define tests:bveq
  (test-suite+
   "Tests for bveq rosette/base/bitvector.rkt"
   (check-bveq-simplifications)))

(define tests:bvslt
  (test-suite+
   "Tests for bvslt rosette/base/bitvector.rkt"
   (check-bvslt-simplifications)
   (check-cmp-semantics @bvslt)))

(define tests:bvsle
  (test-suite+
   "Tests for bvsle rosette/base/bitvector.rkt"
   (check-bvsle-simplifications)
   (check-cmp-semantics @bvsle)))

(define tests:bvult
  (test-suite+
   "Tests for bvult rosette/base/bitvector.rkt"
   (check-bvult-simplifications)
   (check-cmp-semantics @bvult)))

(define tests:bvule
  (test-suite+
   "Tests for bvule rosette/base/bitvector.rkt"
   (check-bvule-simplifications)
   (check-cmp-semantics @bvule)))

(define tests:bvnot
  (test-suite+
   "Tests for bvnot in rosette/base/bitvector.rkt"   
   (check-exn exn:fail? (thunk (@bvnot 1)))
   (check-equal? (@bvnot (@bvnot x)) x)
   (check-equal? (@bvnot (bv -1)) (bv 0))
   (check-equal? (@bvnot (bv 0)) (bv -1))
   (check-semantics @bvnot)))

(define tests:bvor
  (test-suite+
   "Tests for bvor in rosette/base/bitvector.rkt"   
   (check-bitwise-simplifications @bvor @bvand (bv 0))
   (check-semantics @bvor)))
   
(define tests:bvand
  (test-suite+
   "Tests for bvand in rosette/base/bitvector.rkt"   
   (check-bitwise-simplifications @bvand @bvor (bv -1))
   (check-semantics @bvand)))

(define tests:bvand/bvor/bvnot
  (test-suite+
   "Tests for bvand/bvor/bvnot in rosette/base/bitvector.rkt"   
   (check-pe (list (naive @bvnot) (naive* @bvand) (naive* @bvor)))))

(define tests:bvxor
  (test-suite+
   "Tests for bvxor in rosette/base/bitvector.rkt"
   (check-nary @bvxor (bv 0) x y z)
   (check-semantics @bvxor)))

(define tests:bvxor/bvnot
  (test-suite+
   "Tests for bvxor/bvnot rosette/base/bitvector.rkt"   
   (check-pe (list (naive @bvnot) (naive* @bvxor)) (list (bv 0) (bv 5)))))

(define tests:bvshl
  (test-suite+
   "Tests for bvshl in rosette/base/bitvector.rkt"
   (check-shift-simplifications @bvshl)
   (check-semantics @bvshl)))

(define tests:bvlshr
  (test-suite+
   "Tests for bvlshr in rosette/base/bitvector.rkt"
   (check-shift-simplifications @bvlshr)
   (check-semantics @bvlshr)))

(define tests:bvashr
  (test-suite+
   "Tests for bvashr in rosette/base/bitvector.rkt"
   (check-bvashr-simplifications)
   (check-semantics @bvashr)))

(define tests:bvneg
  (test-suite+
   "Tests for bvneg in rosette/base/bitvector.rkt"   
   (check-exn exn:fail? (thunk (@bvneg 1)))
   (check-equal? (@bvneg (@bvneg x)) x)
   (check-semantics @bvneg)))

(define tests:bvadd
  (test-suite+
   "Tests for bvadd in rosette/base/bitvector.rkt"
   (check-bvadd-simplifications)
   (check-semantics @bvadd)))

(define tests:bvadd/bvneg
  (test-suite+
   "Tests for bvadd/bvneg in rosette/base/bitvector.rkt"
   (check-pe (list (naive @bvneg) (naive* @bvadd)) (list (bv 0) (bv 5)))
   (check-pe (list (naive* @bvadd)) (list (bv -1) (bv 2)))))

(define tests:bvsub
  (test-suite+
   "Tests for bvsub in rosette/base/bitvector.rkt"
   (check-semantics @bvsub)))

(define tests:bvmul
  (test-suite+
   "Tests for bvmul in rosette/base/bitvector.rkt"
   (check-bvmul-simplifications)
   (check-semantics @bvmul)))

(define tests:bvudiv
  (test-suite+
   "Tests for bvudiv rosette/base/bitvector.rkt"
   (check-bvudiv-simplifications)
   (check-semantics @bvudiv)))

(define tests:bvsdiv
  (test-suite+
   "Tests for bvsdiv rosette/base/bitvector.rkt"
   (check-bvsdiv-simplifications)
   (check-semantics @bvsdiv)))

(define tests:bvurem
  (test-suite+
   "Tests for bvurem rosette/base/bitvector.rkt"
   (check-bvurem-simplifications)
   (check-semantics @bvurem)))

(time (run-tests tests:bv))
(time (run-tests tests:bveq))
(time (run-tests tests:bvslt))
(time (run-tests tests:bvsle))
(time (run-tests tests:bvult))
(time (run-tests tests:bvule))
(time (run-tests tests:bvnot))
(time (run-tests tests:bvor))
(time (run-tests tests:bvand))
(time (run-tests tests:bvand/bvor/bvnot))
(time (run-tests tests:bvxor))
(time (run-tests tests:bvxor/bvnot))
(time (run-tests tests:bvshl))
(time (run-tests tests:bvlshr))
(time (run-tests tests:bvashr))
(time (run-tests tests:bvneg))
(time (run-tests tests:bvadd))
(time (run-tests tests:bvadd/bvneg))
(time (run-tests tests:bvsub))
(time (run-tests tests:bvmul))
(time (run-tests tests:bvudiv))
(time (run-tests tests:bvsdiv))
(time (run-tests tests:bvurem))
(send solver shutdown)
