#lang racket

(require rackunit rackunit/text-ui racket/generator
         rosette/solver/solution 
         rosette/lib/util/roseunit rosette/solver/smt/z3 
         racket/fixnum 
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/bitvector
         (only-in rosette/base/form/define define-symbolic)
         "exprs.rkt" "common.rkt")

(define solver (new z3%))

(current-bitwidth 4)
(define BV (bitvector 4))
(define-symbolic x y z BV)

(define minval (- (expt 2 (sub1 (bitvector-size BV)))))
(define maxval (expt 2 (sub1 (bitvector-size BV)))) 

(define (solve  . asserts)
  (send/apply solver assert asserts)
  (begin0
    (send solver solve)
    (send solver clear)))

(define (check-semantics op)
  (case (procedure-arity op)
    [(1) 
     (for ([i (in-range minval maxval)])
       (define actual (op (bv i)))
       (define expected 
         ((solve (@bveq (bv i) x)
                 (@bveq y (op x))) y))
       (check-equal? actual expected))]
    [else
     (for* ([i (in-range minval maxval)]
            [j (in-range minval maxval)])
       (define actual (op (bv i) (bv j)))
       (define expected 
         ((solve (@bveq (bv i) x)
                 (@bveq (bv j) y)
                 (@bveq z (op x y))) z))
       (check-equal? actual expected))]))

(define (check-pe ops [consts '()])
  (for ([e (test-exprs 2 ops (list* x y z consts))])  
    (check-pred unsat? (solve (! (@bveq e (reduce e)))))))
 

(define (check-bitwise-simplifications op co id)
  (check-nary op id x y z)
  (check-equal? (op (bv 1) x) (op x (bv 1)))
  (check-equal? (op x (@bvnot x) ) (@bvnot id))
  (check-equal? (op x (@bvnot id)) (@bvnot id))
  (check-equal? (op x (@bvnot id) y z id) (@bvnot id))
  (check-equal? (op (co x y) (@bvnot (co x y))) (@bvnot id))
  (check-equal? (op (op x y) (@bvnot (op x y))) (@bvnot id))
  (check-equal? (op x (@bvnot id)) (@bvnot id))
  (check-equal? (@bvnot (@bvnot (op x y))) (op x y))
  (check-equal? (op z y x x x x y x z z ) (op x y z))
  (check-equal? (op (@bvnot (bv minval)) z y x x x x (bv minval)  y x z z )  (@bvnot id))
  (check-equal? (op z y x x x x (bv minval)  y x z z (bv 2) )  (op (op (bv minval) (bv 2)) x y z))
  (check-equal? (op z y x x x x y x (@bvnot z) z ) (@bvnot id))
  (check-equal? (op z (co z x)  (co z y) x (co x y)) (op x z))
  (check-equal? (op (co x y) (co x y z)) (co x y))
  (check-equal? (op (co x y z) (co x y)) (co x y))
  (check-equal? (op (co x y) (co x y z) x)  x) 
  (check-equal? (op (co x y z) (co x y) x)  x)
  (check-equal? (op (op x (bv minval) z) (@bvnot (bv minval)))  (@bvnot id))
  (check-equal? (op (op x (bv minval) z) (op y (@bvnot (bv minval))))  (@bvnot id)))

(define (check-bvadd-simplifications)
  (check-nary @bvadd (bv 0) x y z)
  (check-equal? (@bvadd (bv 1) x) (@bvadd x (bv 1)))
  (check-equal? (@bvadd x (@bvneg x)) (bv 0))
  (check-equal? (@bvadd (@bvneg x) x) (bv 0))
  (check-equal? (@bvadd (@bvneg (@bvadd x y)) x) (@bvneg y))
  (check-equal? (@bvadd (@bvneg (@bvadd x y)) y) (@bvneg x))
  (check-equal? (@bvadd (@bvadd (@bvneg x) y) x) y)
  (check-equal? (@bvadd (@bvadd x (@bvneg y)) y) x)
  (check-equal? (@bvadd (@bvadd (bv 5) y) (bv -5)) y)
  (check-equal? (@bvadd (@bvadd x y) (@bvneg x)) y)
  (check-equal? (@bvadd (@bvadd x y) (@bvneg y)) x)
  (check-equal? (@bvadd (@bvadd x y z) (@bvadd (@bvneg x) (@bvneg y) z)) z)
  (check-equal? (@bvadd (@bvadd x y z) (@bvadd (@bvneg x) (@bvneg y) (@bvneg z))) (bv 0))
  (check-equal? (@bvadd (@bvadd (bv 1) y z) (@bvadd (bv 1) (@bvneg y) (@bvneg z))) (bv 1))
  (check-equal? (@bvadd (@bvadd (bv 1) y z) (@bvadd (bv -1) (@bvneg y) (@bvneg z))) (bv 0))
  (check-equal? (@bvadd (@bvadd (bv 1) y z) (@bvadd (bv -1) y (@bvneg z))) y)
  (check-equal? (@bvadd x y z (@bvadd (@bvneg x) (@bvneg y))) z)
  (check-equal? (@bvadd x y z (@bvadd (@bvneg x) (@bvneg y)) (@bvneg z)) (bv 0))
  (check-equal? (@bvadd x (bv 0) y (bv 1) z (bv 5)) (@bvadd (bv 6) x y z)))

(define tests:bv
  (test-suite+
   "Tests for bv in rosette/base/bitvector.rkt"
   (check-equal? (bv minval) (bv maxval))
   (check-equal? (bv (sub1 minval)) (bv (sub1 maxval)))))
   
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
   "Tests for soundness of bvand/bvor/bvnot PE rules in rosette/base/bitvector.rkt"   
   (check-pe (list (naive @bvnot) (naive* @bvand) (naive* @bvor)))))

(define tests:bvxor
  (test-suite+
   "Tests for bvxor in rosette/base/bitvector.rkt"
   (check-nary @bvxor (bv 0) x y z)
   (check-semantics @bvxor)))

(define tests:bvxor/bvnot
  (test-suite+
   "Tests for soundness of bvxor/bvnot PE rules in rosette/base/bitvector.rkt"   
   (check-pe (list (naive @bvnot) (naive* @bvxor)) (list (bv 0) (bv 5)))))

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
   (check-semantics @bvadd)
   ))

(define tests:bvadd/bvneg
  (test-suite+
   "Tests for bvadd/bvneg in rosette/base/bitvector.rkt"
   (check-pe (list (naive @bvneg) (naive* @bvadd)) (list (bv 0) (bv 5)))
   ))

(define tests:bvsub
  (test-suite+
   "Tests for bvsub in rosette/base/bitvector.rkt"
   (check-semantics @bvsub)
   ))

(time (run-tests tests:bv))
(time (run-tests tests:bvnot))
(time (run-tests tests:bvor))
(time (run-tests tests:bvand))
(time (run-tests tests:bvand/bvor/bvnot))
(time (run-tests tests:bvxor))
(time (run-tests tests:bvxor/bvnot))
(time (run-tests tests:bvneg))
(time (run-tests tests:bvadd))
(time (run-tests tests:bvadd/bvneg))
(time (run-tests tests:bvsub))
(send solver shutdown)

       
#|
(require rosette/base/core/bitvector)
(require rosette/base/form/define)
(define BV (bitvector 4))
(current-bitwidth 4)
(define-symbolic x y z BV)

|#
