#lang racket

(require rackunit rackunit/text-ui racket/generator (rename-in rackunit [check-exn rackunit/check-exn])
         rosette/solver/solution 
         rosette/lib/roseunit rosette/solver/smt/boolector
         racket/fixnum 
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/result
         (except-in rosette/base/core/bitvector bv)
         (only-in rosette/base/core/bitvector [bv @bv])
         rosette/base/core/polymorphic rosette/base/core/merge 
         (only-in rosette/base/core/equality @equal?)
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/real @= @< @<= @integer?)
         "exprs.rkt" "solver.rkt")

(provide check-state check-bv-exn)

(define BV (bitvector 4))
(define-symbolic x y z BV)
(define-symbolic a b c d e f g @boolean?)


(define minval (- (expt 2 (sub1 (bitvector-size BV)))))
(define maxval+1 (expt 2 (sub1 (bitvector-size BV)))) 
(define maxval (sub1 maxval+1))
(define (bv v [t BV]) (@bv v t))

(define-syntax-rule (check-exn e ...)
  (begin
    (rackunit/check-exn e ...)
    (clear-vc!)))

(define-syntax-rule (define-tests (name arg ...) body ...)
  (define (name arg ...)
    (begin
     (clear-vc!)
     body ...
     (clear-vc!))))

(define-tests (check-nary op id x y z)
  (check-equal? (op id id) id)
  (check-equal? (op id id id) id)
  (check-equal? (op x) x)
  (check-equal? (op id x) x)
  (check-equal? (op x id) x)
  (check-equal? (op x id y) (op x y))
  (check-equal? (op x y) (op y x))
)

(define-tests (check-semantics op)
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

(define-tests (check-cmp-semantics op)
  (for* ([i (in-range minval maxval+1)]
            [j (in-range minval maxval+1)])
       (define actual (op (bv i) (bv j)))
       (define expected 
         ((solve (@bveq (bv i) x)
                 (@bveq (bv j) y)
                 (@equal? a (op x y))) a))
       (check-equal? actual expected)))


(define-tests (check-pe ops [consts '()])
  (define es (test-exprs 2 ops (list* x y z consts)))
  ;(printf "exprs: ~a\n" (length es))
  (for ([e es])
   ;(printf "~e = ~e\n" e (reduce e))
    (check-pred unsat? (solve (! (@bveq e (reduce e)))))))

(define-syntax-rule (check-valid? (op e ...) expected)
  (let ([actual (op e ...)])
    (check-equal? actual expected)
    (check-pred unsat? (solve (! (@equal? (expression op e ...) expected))))))

(define-tests (check-bveq-simplifications)
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
                
(define-tests (check-bvcmp-simplifications l* g*)
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

(define-tests (check-bvslt-simplifications)
  (check-bvcmp-simplifications @bvslt @bvsgt)
  (check-valid? (@bvslt x (bv maxval)) (! (@bveq x (bv maxval))))
  (check-valid? (@bvslt (bv maxval) x) #f)
  (check-valid? (@bvslt x (bv minval)) #f)
  (check-valid? (@bvslt (bv minval) x) (! (@bveq x (bv minval)))))

(define-tests (check-bvsle-simplifications)
  (check-bvcmp-simplifications @bvsle @bvsge)
  (check-valid? (@bvsle x (bv maxval)) #t)
  (check-valid? (@bvsle (bv maxval) x) (@bveq x (bv maxval)))
  (check-valid? (@bvsle x (bv minval)) (@bveq x (bv minval)))
  (check-valid? (@bvsle (bv minval) x) #t))

(define-tests (check-bvult-simplifications)
  (check-bvcmp-simplifications @bvult @bvugt)
  (check-valid? (@bvult x (bv -1)) (! (@bveq x (bv -1))))
  (check-valid? (@bvult (bv -1) x) #f)
  (check-valid? (@bvult x (bv 0)) #f)
  (check-valid? (@bvult (bv 0) x) (! (@bveq x (bv 0)))))

(define-tests (check-bvule-simplifications)
  (check-bvcmp-simplifications @bvule @bvuge)
  (check-valid? (@bvule x (bv -1)) #t)
  (check-valid? (@bvule (bv -1) x) (@bveq x (bv -1)))
  (check-valid? (@bvule x (bv 0)) (@bveq x (bv 0)))
  (check-valid? (@bvule (bv 0) x) #t))

(define-tests (check-bitwise-simplifications op co id)
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

(define-tests (check-shift-simplifications op)
  (check-valid? (op x (bv 0)) x)
  (check-valid? (op (bv 0) x) (bv 0))
  (check-valid? (op x (bv 4)) (bv 0))
  (check-valid? (op x (bv 5)) (bv 0))
  (check-valid? (op x (bv -1)) (bv 0)))

(define-tests (check-bvashr-simplifications)
  (check-valid? (@bvashr x (bv 0)) x)
  (check-valid? (@bvashr (bv 0) x) (bv 0))
  (check-valid? (@bvashr (bv -1) x) (bv -1))
  (check-valid? (@bvashr x (bv 4)) (ite (@bveq (bv 0) (@bvand x (bv minval))) (bv 0) (bv -1)))
  (check-valid? (@bvashr x (bv 5)) (ite (@bveq (bv 0) (@bvand x (bv minval))) (bv 0) (bv -1)))
  (check-valid? (@bvashr x (bv -1)) (ite (@bveq (bv 0) (@bvand x (bv minval))) (bv 0) (bv -1))))

(define-tests (check-bvadd-simplifications)
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

(define-tests (check-bvmul-simplifications)
  (check-nary @bvmul (bv 1) x y z)
  (check-valid? (@bvmul (bv 3) x) (@bvmul x (bv 3)))
  (check-valid? (@bvmul (bv 0) x) (bv 0))
  (check-valid? (@bvmul x (bv 0)) (bv 0))
  (check-valid? (@bvmul x (bv -1)) (@bvneg x))
  (check-valid? (@bvmul (bv -1) x) (@bvneg x))
  (check-valid? (@bvmul (@bvmul x (bv 2)) (bv 3)) (@bvmul (bv 6) x)))

(define-tests (check-bvudiv-simplifications)
  (check-valid? (@bvudiv (bv 3) (bv 0)) (bv -1))
  (check-valid? (@bvudiv x (bv 0)) (bv -1))
  (check-valid? (@bvudiv (bv 3) (bv 1)) (bv 3))
  (check-valid? (@bvudiv x (bv 1)) x)
  (check-valid? (@bvudiv x (bv -1)) (ite (@bveq x (bv -1)) (bv 1) (bv 0)))
  (check-valid? (@bvudiv (bv 0) x) (ite (@bveq x (bv 0)) (bv -1) (bv 0)))
  (check-valid? (@bvudiv x x) (ite (@bveq x (bv 0)) (bv -1) (bv 1)))
  (check-valid? (@bvudiv (ite b (bv 6) (bv 8)) (bv 2)) (ite b (bv 3) (bv 4)))
  (check-valid? (@bvudiv (bv 6) (ite b (bv 2) (bv 3))) (ite b (bv 3) (bv 2))))

(define-tests (check-bvsdiv-simplifications)
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

(define-tests (check-bvurem-simplifications)
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

(define-tests (check-signed-remainder-simplifications op)
  (check-valid? (op (bv 3) (bv 1)) (bv 0))
  (check-valid? (op (bv 0) (bv 1)) (bv 0))
  (check-valid? (op (bv -3) (bv 1)) (bv 0))
  (check-valid? (op x (bv 1)) (bv 0))
  (check-valid? (op (@bvadd x y) (bv 1)) (bv 0))
  (check-valid? (op (bv 3) (bv -1)) (bv 0))
  (check-valid? (op (bv 0) (bv -1)) (bv 0))
  (check-valid? (op (bv -3) (bv -1)) (bv 0))
  (check-valid? (op x (bv -1)) (bv 0))
  (check-valid? (op (@bvadd x y) (bv -1)) (bv 0))
  (check-valid? (op (bv 3) (bv 0)) (bv 3))
  (check-valid? (op (bv 0) (bv 0)) (bv 0))
  (check-valid? (op (bv -3) (bv 0)) (bv -3))
  (check-valid? (op x (bv 0)) x)
  (check-valid? (op (@bvadd x y) (bv 0)) (@bvadd x y))
  (check-valid? (op (bv 0) (bv 3)) (bv 0))
  (check-valid? (op (bv 0) (bv -3)) (bv 0))
  (check-valid? (op (bv 0) x) (bv 0))
  (check-valid? (op (bv 0) (@bvadd x y)) (bv 0))
  (check-valid? (op x x) (bv 0))
  (check-valid? (op x (@bvneg x)) (bv 0))
  (check-valid? (op (@bvneg x) x) (bv 0))
  (check-valid? (op (ite b (bv 6) (bv 4)) (bv 3)) (ite b (bv 0) (bv 1)))
  (check-valid? (op (bv 6) (ite b (bv 4) (bv 5))) (ite b (bv 2) (bv 1))))

(define-tests (check-bvsrem-simplifications)
  (check-signed-remainder-simplifications @bvsrem)
  (check-valid? (@bvsrem x (bv minval)) (ite (@bveq x (bv minval)) (bv 0) x))
  (check-valid? (@bvsrem (@bvadd x y) (bv minval)) (ite (@bveq (@bvadd x y) (bv minval)) (bv 0) (@bvadd x y))))

(define-tests (check-concat-semantics)
  (for* ([i (in-range minval maxval+1)]
         [j (in-range minval maxval+1)])
    (define BVi (bitvector (max 1 (integer-length i))))
    (define BVj (bitvector (max 1 (integer-length j))))
    (define BVo (bitvector (+ (bitvector-size BVi) (bitvector-size BVj))))
    (define-symbolic* vi BVi)
    (define-symbolic* vj BVj)
    (define-symbolic* vo BVo)
    (define actual (@concat (bv i BVi) (bv j BVj)))
    ;(printf "(~a ~a ~a) = ~a\n" @concat (bv i BVi) (bv j BVj) actual)
    (define expected 
      ((solve (@bveq (bv i BVi) vi)
              (@bveq (bv j BVj) vj)
              (@bveq (@concat vi vj) vo)) vo))
    (check-equal? actual expected)))

(define-tests (check-extract-semantics)
  (for* ([i 4]
         [j (in-range 0 (add1 i))]
         [x (in-range minval maxval+1)])
    (define BVo (bitvector (add1 (- i j))))
    (define-symbolic* vi vj @integer?)
    (define-symbolic* vo BVo)
    (define actual (@extract i j (bv x)))
    ;(printf "(~a ~a ~a) = ~a\n" @concat (bv i BVi) (bv j BVj) actual)
    (define expected 
      ((solve (@= i vi)
              (@= j vj)
              (@bveq (@extract vi vj (bv x)) vo)) vo))
    (check-equal? actual expected)))

(define-tests (check-concat-simplifications)
  (check-valid? (@concat (@extract 3 2 x) (@extract 1 1 x)) (@extract 3 1 x)))

(define-tests (check-extract-simplifications)
  (check-valid? (@extract 3 0 x) x)
  (check-valid? (@extract 3 0 (@bvadd x y)) (@bvadd x y))
  (check-valid? (@extract 2 1 (@extract 3 1 x)) (@extract 3 2 x))
  (check-valid? (@extract 0 0 (@concat x (bv 1 1))) (bv 1 1))
  (check-valid? (@extract 3 0 (@concat x y)) y)
  (check-valid? (@extract 2 0 (@concat x y)) (@extract 2 0 y))
  (check-valid? (@extract 7 4 (@concat x y)) x)
  (check-valid? (@extract 7 5 (@concat x y)) (@extract 3 1 x))
  (check-valid? (@extract 2 0 (@sign-extend x (bitvector 8))) (@extract 2 0 x))
  (check-valid? (@extract 2 0 (@zero-extend x (bitvector 8))) (@extract 2 0 x))
  (check-valid? (@extract 3 0 (@sign-extend x (bitvector 8))) x)
  (check-valid? (@extract 3 0 (@zero-extend x (bitvector 8))) x)
  (check-valid? (@extract 4 0 (@sign-extend x (bitvector 8))) (@sign-extend x (bitvector 5)))
  (check-valid? (@extract 4 0 (@zero-extend x (bitvector 8))) (@zero-extend x (bitvector 5)))
  (define-symbolic* n @integer?)
  (check-equal? (type-of (@extract n n x)) (bitvector 1))
  )

(define-tests (check-extend-simplifications op)
  (check-valid? (op x BV) x)
  (check-valid? (op (op x (bitvector 6)) (bitvector 8)) (op x (bitvector 8))))

(define-tests (check-extend-semantics op)
  (for* ([t (in-range 4 8)]
         [v (in-range minval maxval+1)])
    (define BVo (bitvector t))
    (define-symbolic* vo BVo)
    (define actual (op (bv v BV) BVo))
    (define expected
      ((solve (@bveq x (bv v BV))
              (@bveq vo (op x BVo))) vo))
    (check-equal? actual expected)))
    
(define-tests (check-bv->*-semantics op)
  (for* ([v (in-range minval maxval+1)])
    (define actual (op (bv v BV)))
    (define-symbolic* out @integer?)
    (define expected
      ((solve (@bveq x (bv v BV))
              (@= out (op x))) out))
    (check-equal? actual expected)))

(define-tests (check-integer->bitvector-semantics)
  (for* ([v (in-range (* 2 minval) (* 2 maxval+1))])
    (define actual (@integer->bitvector v BV))
    (define-symbolic* out BV)
    (define-symbolic* in @integer?)
    (define expected
      ((solve (@= in v)
              (@bveq out (@integer->bitvector in BV))) out))
    (check-equal? actual expected)))
 
(define-tests (check-lifted-bv-type)
  (define-symbolic* n @integer?)
  (check-exn #px"exact-positive-integer\\?" (thunk (bitvector 0)))
  (check-exn #px"exact-positive-integer\\?" (thunk (bitvector -1)))
  (check-exn #px"exact-positive-integer\\?" (thunk (bitvector n)))
  (check-eq? (bitvector 3) (bitvector 3))
  (check-exn #px"real, non-infinite, non-NaN number" (thunk (@bv +inf.0 BV)))
  (check-exn #px"exact-positive-integer\\? or bitvector\\? type" (thunk (@bv 1 3.2)))
  (check-exn #px"exact-positive-integer\\? or bitvector\\? type" (thunk (@bv 1 @integer?))))

(define (phi . xs) (apply merge* xs))

(define-syntax-rule (check-state actual expected-value expected-asserts)
  (let* ([r (with-vc actual)]
         [v expected-value]
         [a (apply && expected-asserts)])
    (check-equal? (result-value r) v)
    (check-equal? (vc-assumes (result-state r)) #t)
    (or (equal? (vc-asserts (result-state r)) a)
        (check-pred unsat? (solve (! (<=> (vc-asserts (result-state r)) a)))))))

(define-syntax check-bv-exn
  (syntax-rules ()
    [(_ expr) (check-bv-exn #px"expected bitvectors of same length" expr)]
    [(_ p expr)
     (match (with-vc expr)
       [(failed e _)
        (define rx p)
        (cond [(procedure? p) (check-pred p e)]
              [else (check-pred exn:fail? e)
                    (check-true (regexp-match? rx (exn-message e)))])]
       [r (check-pred failed? r)])]))
         
(define-tests (check-lifted-unary)
  (define-symbolic* n @integer?)
  (check-not-exn (thunk (@bvnot (bv 1))))
  (check-bv-exn (@bvnot n))
  (check-bv-exn (@bvnot (phi (cons a 1) (cons b '()))))
  (check-state (@bvnot (phi (cons a 1) (cons b (bv -1)))) (bv 0) (list b))
  (check-state (@bvnot (phi (cons a 1) (cons b x) (cons c '()) (cons d (bv -1 2))))
               (phi (cons b (@bvnot x)) (cons d (bv 0 2))) (list (|| b d)))
  (check-state (@bvnot (phi (cons a 1) (cons b x) (cons c '()) (cons d (bv -1))))
               (@bvnot (phi (cons b x) (cons d (bv -1 4)))) (list (|| b d)))
  (check-state (@bvnot (phi (cons a (bv 0 2)) (cons b (bv -1))))
               (phi (cons a (bv -1 2)) (cons b (bv 0))) (list)))


(define-tests (check-lifted-binary)
  (define-symbolic* n @integer?)
  (check-not-exn (thunk (@bvsdiv x y)))
  (check-bv-exn (@bvsdiv x 1))
  (check-bv-exn (@bvsdiv 1 x))
  (check-state (@bvsdiv x (phi (cons a 1) (cons b y) (cons c (bv 1 2)))) 
               (@bvsdiv x y) (list b))
  (check-state (@bvsdiv (phi (cons a 1) (cons b y) (cons c (bv 1 2))) x) 
               (@bvsdiv y x) (list b))
  (check-bv-exn (@bvsdiv (phi (cons a 1) (cons b #f)) (phi (cons c '()) (cons d 3))))
  (check-bv-exn (@bvsdiv (phi (cons a 1) (cons b x)) (phi (cons c '()) (cons d 3))))
  (check-bv-exn (@bvsdiv (phi (cons a 1) (cons b #f)) (phi (cons c '()) (cons d x))))
  (check-bv-exn (@bvsdiv (phi (cons a 1) (cons b x)) 
                         (phi (cons c '()) (cons d (bv 1 2)))))
  (check-state (@bvsdiv (phi (cons a 1) (cons b x)) (phi (cons c y) (cons d '())))
               (@bvsdiv x y) (list (&& b c)))
  (check-state (@bvsdiv (phi (cons a (bv 1 2)) (cons b x)) 
                        (phi (cons c y) (cons d '())))
               (@bvsdiv x y) (list (&& b c)))
  (check-state (@bvsdiv (phi (cons a (bv 1 2)) (cons b x) (cons e 'e)) 
                        (phi (cons f "f") (cons c y) (cons d '())))
               (@bvsdiv x y) (list (&& b c)))
  (check-state (@bvsdiv (phi (cons a (bv 6 8)) (cons b x)) 
                        (phi (cons c y) (cons d (bv 2 8))))
               (phi (cons (&& b c) (@bvsdiv x y))
                    (cons (&& a d) (bv 3 8))) 
               (list  (|| (&& b c) (&& a d))))
  (check-state (@bvsdiv (phi (cons a (bv 6 8)) (cons b x)) 
                        (phi (cons c y) (cons d (bv 2 8)) (cons e 'e)))
               (phi (cons (&& b c) (@bvsdiv x y))
                    (cons (&& a d) (bv 3 8))) 
               (list (|| (&& b c) (&& a d))))
  (check-state (@bvsdiv (phi (cons a (bv 6 8)) (cons b x) (cons e 'e)) 
                        (phi (cons c y) (cons d (bv 2 8))))
               (phi (cons (&& b c) (@bvsdiv x y))
                    (cons (&& a d) (bv 3 8))) 
               (list (|| (&& b c) (&& a d))))
  )

(define-tests (check-@bvadd-exn bad-arg)
  (check-bv-exn exn:fail? (@bvadd bad-arg x y))
  (check-bv-exn exn:fail? (@bvadd x bad-arg y))
  (check-bv-exn exn:fail? (@bvadd x y bad-arg)))

(define-tests (check-lifted-nary)
  (define-symbolic* n @integer?)
  (check-not-exn (thunk (@bvadd x y z)))
  (check-@bvadd-exn 1)
  (check-@bvadd-exn n)
  (check-@bvadd-exn (phi (cons a 1) (cons b (bv 2 2))))
  (check-state (@bvadd z (phi (cons a 1) (cons b x)) (phi (cons c y) (cons d '())))
               (@bvadd z x y) (list b c))
  (check-state (@bvadd z (phi (cons a 1) (cons b x)) y)
               (@bvadd z x y) (list b))
  (check-bv-exn (@bvadd (phi (cons a 1) (cons b x)) 
                        (phi (cons c 2) (cons d #f))
                        (phi (cons e 'e) (cons f (bv 1 2)))))
  (check-bv-exn (@bvadd (phi (cons a 1) (cons b x)) 
                        (phi (cons c 2) (cons d #f))
                        (phi (cons e y) (cons f (bv 1 2)))))
  (check-bv-exn (@bvadd (phi (cons a 1) (cons b x)) 
                        (phi (cons c y) (cons d #f))
                        (phi (cons e 'e) (cons f (bv 1 2)))))
  (check-state (@bvadd (phi (cons a x) (cons b (bv 1 8))) 
                       (phi (cons c y) (cons d (bv 2 8)))
                       (phi (cons e z) (cons f (bv 3 8))))
               (phi (cons (&& a c e) (@bvadd x y z))
                    (cons (&& b d f) (bv 6 8)))
               (list (|| (&& a c e) (&& b d f))))
  (check-state (@bvadd (phi (cons a 1) (cons b (bv 1 8))) 
                       (phi (cons c y) (cons d (bv 2 8)))
                       (phi (cons e z) (cons f (bv 3 8))))
               (bv 6 8)
               (list (&& b d f)))
  (check-state (@bvadd (phi (cons a x) (cons b (bv 1 8))) 
                       (phi (cons c 1) (cons d (bv 2 8)))
                       (phi (cons e z) (cons f (bv 3 8))))
               (bv 6 8)
               (list (&& b d f)))
  (check-state (@bvadd (phi (cons a x) (cons b (bv 1 8))) 
                       (phi (cons c y) (cons d (bv 2 8)))
                       (phi (cons e 1) (cons f (bv 3 8))))
               (bv 6 8)
               (list (&& b d f)))
  (check-state (@bvadd (phi (cons a x) (cons b (bv 1 8))) 
                       (phi (cons c y) (cons d (bv 2 8)) (cons g 'g))
                       (phi (cons e z) (cons f (bv 3 8))))
               (phi (cons (&& a c e) (@bvadd x y z))
                    (cons (&& b d f) (bv 6 8)))
               (list (|| (&& a c e) (&& b d f))))
  (check-state (@bvadd (phi (cons a x) (cons b (bv 1 8))) 
                       (phi (cons c y) (cons d (bv 2 8)) )
                       (phi (cons e z) (cons f (bv 3 8)) (cons g 'g)))
               (phi (cons (&& a c e) (@bvadd x y z))
                    (cons (&& b d f) (bv 6 8)))
               (list (|| (&& a c e) (&& b d f))))
  (check-state (@bvadd (phi (cons a x) (cons g 'g) (cons b (bv 1 8))) 
                       (phi (cons c y) (cons d (bv 2 8)) )
                       (phi (cons e z) (cons f (bv 3 8))))
               (phi (cons (&& a c e) (@bvadd x y z))
                    (cons (&& b d f) (bv 6 8)))
               (list (|| (&& a c e) (&& b d f)))))

(define-tests (check-lifted-concat)
  (define-symbolic* n @integer?)
  (check-not-exn (thunk (@concat x)))
  (check-not-exn (thunk (@concat x y)))
  (check-not-exn (thunk (@concat x y z)))
  (check-bv-exn exn:fail? (@concat 1))
  (check-bv-exn exn:fail? (@concat 1 x))
  (check-bv-exn exn:fail? (@concat x n))
  (check-bv-exn exn:fail? (@concat x (phi (cons a 1) (cons b '()))))
  (check-bv-exn exn:fail? (@concat (phi (cons a 1) (cons b '())) x))
  (check-state (@concat x (phi (cons a 1) (cons b (bv 2 8))))
               (@concat x (bv 2 8)) (list b))
  (check-state (@concat (phi (cons a 1) (cons b (bv 2 8))) x)
               (@concat (bv 2 8) x) (list b))
  (check-state (@concat x (phi (cons a 1) (cons b (bv 2 8)) (cons c y)))
               (phi (cons b (@concat x (bv 2 8)))
                    (cons c (@concat x y))) 
               (list (|| b c)))
  (check-state (@concat x (phi (cons a 1) (cons b (bv 2 8)) (cons c y)) z)
               (phi (cons b (@concat x (bv 2 8) z))
                    (cons c (@concat x y z))) 
               (list (|| b c)))
  (check-state (@concat (phi (cons a x) (cons b (bv 2 8))) 
                        (phi (cons c y) (cons d (bv 1 2))))
               (phi (cons (&& a c) (@concat x y))
                    (cons (&& a d) (@concat x (bv 1 2)))
                    (cons (&& b c) (@concat (bv 2 8) y))
                    (cons (&& b d) (@concat (bv 2 8) (bv 1 2))))
               (list))
   (check-state (@concat (phi (cons a x) (cons b 1)) 
                        (phi (cons c y) (cons d (bv 1 2))))
               (phi (cons c (@concat x y))
                    (cons d (@concat x (bv 1 2))))
               (list a))
  (check-state (@concat (phi (cons a x) (cons b 1)) 
                        (phi (cons c y) (cons d 2)))
               (@concat x y)
               (list a c)) 
  (check-state (@concat (phi (cons a x) (cons e '3)) 
                        (phi (cons c y) (cons d (bv 1 2)) (cons f 'f)))
               (phi (cons c (@concat x y))
                    (cons d (@concat x (bv 1 2))))
               (list a (|| c d)) ))

(define-tests (check-lifted-extract)
  (define-symbolic* i j @integer?)
  (check-bv-exn #px"expected integer\\?" (@extract "1" 2 x))
  (check-bv-exn #px"expected integer\\?" (@extract 1 "2" x))
  (check-bv-exn #px"expected: bitvector\\?" (@extract 2 1 (phi (cons a 3) (cons b '()))))
  (check-bv-exn #px"expected integer\\?" (@extract 2.3 2 x))  
  (check-bv-exn #px"expected integer\\?" (@extract 2 1.9 x))  
  (check-bv-exn #px"expected i >= j" (@extract 2 3 x))  
  (check-bv-exn #px"expected j >= 0" (@extract 2 -1 x))
  (check-bv-exn #px"expected \\(size-of x\\) > i" (@extract 4 1 x))
  (check-bv-exn exn:fail? (@extract 4 1 (phi (cons a x) (cons b 1))))
  (check-state (@extract 2 1 x) (@extract 2 1 x) (list))
  (check-state (@extract i 3 x) (@extract 3 3 x) (list (@< i 4) (@<= 3 i)))
  (check-state (@extract i 2 x) 
               (phi (cons (@= i 3) (@extract 3 2 x))
                    (cons (@= i 2) (@extract 2 2 x)))
               (list (@< i 4) (@<= 2 i)))
  (check-state (@extract 0 j x) (@extract 0 0 x) (list (@<= j 0) (@<= 0 j)))
  (check-state (@extract 1 j x) 
               (phi (cons (@= j 0) (@extract 1 0 x))
                    (cons (@= j 1) (@extract 1 1 x)))
               (list (@<= j 1) (@<= 0 j)))
  (check-state (@extract i j (bv 2 2))
               (phi (cons (&& (@= i 1) (@= j 1)) (@extract 1 1 (bv 2 2)))
                    (cons (&& (@= i 1) (@= j 0)) (@extract 1 0 (bv 2 2)))
                    (cons (&& (@= i 0) (@= j 0)) (@extract 0 0 (bv 2 2))))
               (list (@<= 0 j) (@<= j i) (@< i 2)))
  (check-state (@extract 2 1 (phi (cons a x) (cons b 3))) 
               (@extract 2 1 x) (list a))
  (check-state (@extract i 3 (phi (cons a x) (cons b 3) (cons c y) (cons d '()))) 
               (@extract 3 3 (phi (cons a x)  (cons c y)))
               (list  (@<= 3 i) (@< i 4) (|| a c)))
  (check-state (@extract i 2 (phi (cons a x) (cons b 3) (cons c y) (cons d '()))) 
               (phi (cons (@= i 3) (@extract 3 2 (phi (cons a x)  (cons c y))))
                    (cons (@= i 2) (@extract 2 2 (phi (cons a x)  (cons c y)))))
               (list  (@<= 2 i) (@< i 4) (|| a c)))
  (check-state (@extract 0 j (phi (cons a x) (cons b 3) (cons c y) (cons d '()))) 
               (@extract 0 0 (phi (cons a x)  (cons c y))) 
               (list (@<= 0 j) (|| a c) (@<= j 0)))
  (check-state (@extract 1 j (phi (cons a x) (cons b 3) (cons c y) (cons d '())))  
               (phi (cons (@= j 0) (@extract 1 0 (phi (cons a x)  (cons c y)))) 
                    (cons (@= j 1) (@extract 1 1 (phi (cons a x)  (cons c y))))) 
               (list (@<= j 1) (@<= 0 j) (|| a c)))
  (check-state (@extract i j (phi (cons a (bv 2 2)) (cons b 3) (cons c (bv 1 1))))
               (phi (cons (&& a (&& (@= i 1) (@= j 0))) (@extract 1 0 (bv 2 2)))
                    (cons (|| c (&& a (|| (&& (@= i 1) (@= j 1)) (&& (@= i 0) (@= j 0)))))
                          (phi (cons (&& a (|| (&& (@= i 1) (@= j 1)) (&& (@= i 0) (@= j 0))))
                                     (phi (cons (&& (@= i 1) (@= j 1)) (@extract 1 1 (bv 2 2)))
                                          (cons (&& (@= i 0) (@= j 0)) (@extract 0 0 (bv 2 2)))))
                               (cons c (@extract 0 0 (bv 1 1))))))
               (list (|| a c) (|| (@< i 1) (! c)) (|| (@< i 2) (! a)) (@<= 0 j) (@<= j i)))           
  )
    
(define-tests (check-lifted-extend)
  (define err #px"expected \\(bitvector-size t\\) >= \\(bitvector-size \\(get-type v\\)\\)")
  (check-bv-exn #px"expected: bitvector\\?" (@sign-extend 1 BV))
  (check-bv-exn err (@sign-extend x 1))
  (check-bv-exn err (@sign-extend x (bitvector 2)))
  (check-bv-exn err (@sign-extend (phi (cons a x) (cons b (bv 3 3))) (bitvector 2)))
  (check-bv-exn err (@sign-extend x (phi (cons a (bitvector 2)) (cons b 1))))
  (check-bv-exn err (@sign-extend (phi (cons a x) (cons b (bv 3 3))) 
                                  (phi (cons c (bitvector 2)) (cons d (bitvector 1)))))
  (check-state (@sign-extend x (bitvector 5)) (@sign-extend x (bitvector 5)) (list))
  (check-state (@sign-extend (phi (cons a x) (cons b (bv 3 3))) BV)
               (phi (cons a x) (cons b (@sign-extend (bv 3 3) BV))) 
               (list))
  (check-state (@sign-extend (phi (cons a x) (cons b (bv 3 5))) BV)
               x
               (list a))
  (check-state (@sign-extend (phi (cons a x) (cons b (bv 3 3)) (cons c 1)) BV)
               (phi (cons a x) (cons b (@sign-extend (bv 3 3) BV))) 
               (list (|| a b)))
  (check-state (@sign-extend x (phi (cons a BV) (cons b (bitvector 5))))
               (phi (cons a x) (cons b (@sign-extend x (bitvector 5))))
               (list))
  (check-state (@sign-extend x (phi (cons a BV) (cons b (bitvector 3))))
               x
               (list a))
  (check-state (@sign-extend x (phi (cons a BV) (cons b (bitvector 5)) (cons c (bitvector 3)) (cons d 1)))
               (phi (cons a x) (cons b (@sign-extend x (bitvector 5))))
               (list (|| a b)))
  (check-state (@sign-extend (phi (cons a x) (cons b (bv 3 3)))
                             (phi (cons c BV) (cons d (bitvector 5))))
               (phi (cons (&& a c) x) 
                    (cons (&& a d) (@sign-extend x (bitvector 5)))
                    (cons (&& b c) (@sign-extend (bv 3 3) BV))
                    (cons (&& b d) (@sign-extend (bv 3 3) (bitvector 5))))
               (list))
  (check-state (@sign-extend (phi (cons a x) (cons b (bv 3 3)) (cons e 'e))
                             (phi (cons c BV) (cons d (bitvector 5))))
               (phi (cons (&& a c) x) 
                    (cons (&& a d) (@sign-extend x (bitvector 5)))
                    (cons (&& b c) (@sign-extend (bv 3 3) BV))
                    (cons (&& b d) (@sign-extend (bv 3 3) (bitvector 5))))
               (list (|| a b)))
  (check-state (@sign-extend (phi (cons a x) (cons b (bv 3 3)))
                             (phi (cons c BV) (cons d (bitvector 5)) (cons e 'e)))
               (phi (cons (&& a c) x) 
                    (cons (&& a d) (@sign-extend x (bitvector 5)))
                    (cons (&& b c) (@sign-extend (bv 3 3) BV))
                    (cons (&& b d) (@sign-extend (bv 3 3) (bitvector 5))))
               (list (|| (&& a c) (&& a d) (&& b c) (&& b d))))
  (check-state (@sign-extend (phi (cons a x) (cons b (bv 8 8)))
                             (phi (cons c BV) (cons d (bitvector 1)) (cons e 'e)))
               x
               (list (&& a c))))
  
(define-tests (check-lifted-integer->bitvector)
  (check-bv-exn exn:fail? (@integer->bitvector "3" BV))
  (check-bv-exn #px"expected a bitvector type" (@integer->bitvector 3 3))
  (check-bv-exn #px"expected a bitvector type" (@integer->bitvector 3 (phi (cons a 1) (cons b "3"))))
  (check-state (@integer->bitvector 3 BV) (bv 3 4) (list))
  (check-state (@integer->bitvector (phi (cons a 3) (cons b "3")) BV) (bv 3 4) (list a))
  (check-state (@integer->bitvector 3 (phi (cons a BV) (cons b (bitvector 3))))
               (phi (cons a (bv 3 4)) (cons b (bv 3 3))) (list))
  (check-state (@integer->bitvector 3 (phi (cons a BV) (cons b (bitvector 3)) (cons c '())))
               (phi (cons a (bv 3 4)) (cons b (bv 3 3))) (list (|| a b))))

(define-tests (check-lifted-bv->*)
  (check-bv-exn #px"expected: bitvector\\?" (@bitvector->integer 3))
  (check-state (@bitvector->integer (bv 3 3)) 3 (list))
  (check-state (@bitvector->integer (phi (cons a (bv 3 4)) (cons b (bv 3 3)))) 
               (phi (cons a 3) (cons b 3)) (list))
  (check-state (@bitvector->integer (phi (cons a (bv 3 4)) (cons b 3)))
               3 (list a))
  (check-state (@bitvector->integer (phi (cons a (bv 3 4)) (cons b (bv 3 3)) (cons c '()))) 
               (phi (cons a 3) (cons b 3)) (list (|| a b))))
  

(define tests:bv
  (test-suite+
   "Tests for bv in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-equal? (bv minval) (bv maxval+1))
   (check-equal? (bv (sub1 minval)) (bv (sub1 maxval+1)))))

(define tests:bveq
  (test-suite+
   "Tests for bveq rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bveq-simplifications)))

(define tests:bvslt
  (test-suite+
   "Tests for bvslt rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvslt-simplifications)
   (check-cmp-semantics @bvslt)))

(define tests:bvsle
  (test-suite+
   "Tests for bvsle rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvsle-simplifications)
   (check-cmp-semantics @bvsle)))

(define tests:bvult
  (test-suite+
   "Tests for bvult rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvult-simplifications)
   (check-cmp-semantics @bvult)))

(define tests:bvule
  (test-suite+
   "Tests for bvule rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvule-simplifications)
   (check-cmp-semantics @bvule)))

(define tests:bvnot
  (test-suite+
   "Tests for bvnot in rosette/base/bitvector.rkt"   
   #:features '(qf_bv)
   (check-exn exn:fail? (thunk (@bvnot 1)))
   (check-equal? (@bvnot (@bvnot x)) x)
   (check-equal? (@bvnot (bv -1)) (bv 0))
   (check-equal? (@bvnot (bv 0)) (bv -1))
   (check-semantics @bvnot)))

(define tests:bvor
  (test-suite+
   "Tests for bvor in rosette/base/bitvector.rkt"   
   #:features '(qf_bv)
   (check-bitwise-simplifications @bvor @bvand (bv 0))
   (check-semantics @bvor)))
   
(define tests:bvand
  (test-suite+
   "Tests for bvand in rosette/base/bitvector.rkt"   
   #:features '(qf_bv)
   (check-bitwise-simplifications @bvand @bvor (bv -1))
   (check-semantics @bvand)))

(define tests:bvand/bvor/bvnot
  (test-suite+
   "Tests for bvand/bvor/bvnot in rosette/base/bitvector.rkt"   
   #:features '(qf_bv)
   (check-pe (list (naive @bvnot) (naive* @bvand) (naive* @bvor)))))

(define tests:bvxor
  (test-suite+
   "Tests for bvxor in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-nary @bvxor (bv 0) x y z)
   (check-semantics @bvxor)))

(define tests:bvxor/bvnot
  (test-suite+
   "Tests for bvxor/bvnot rosette/base/bitvector.rkt"   
   #:features '(qf_bv)
   (check-pe (list (naive @bvnot) (naive* @bvxor)) (list (bv 0) (bv 5)))))

(define tests:bvshl
  (test-suite+
   "Tests for bvshl in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-shift-simplifications @bvshl)
   (check-semantics @bvshl)))

(define tests:bvlshr
  (test-suite+
   "Tests for bvlshr in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-shift-simplifications @bvlshr)
   (check-semantics @bvlshr)))

(define tests:bvashr
  (test-suite+
   "Tests for bvashr in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvashr-simplifications)
   (check-semantics @bvashr)))

(define tests:bvneg
  (test-suite+
   "Tests for bvneg in rosette/base/bitvector.rkt"   
   #:features '(qf_bv)
   (check-exn exn:fail? (thunk (@bvneg 1)))
   (check-equal? (@bvneg (@bvneg x)) x)
   (check-semantics @bvneg)))

(define tests:bvadd
  (test-suite+
   "Tests for bvadd in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvadd-simplifications)
   (check-semantics @bvadd)))

(define tests:bvadd/bvneg
  (test-suite+
   "Tests for bvadd/bvneg in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-pe (list (naive @bvneg) (naive* @bvadd)) (list (bv 0) (bv 5)))
   (check-pe (list (naive* @bvadd)) (list (bv -1) (bv 2)))))

(define tests:bvsub
  (test-suite+
   "Tests for bvsub in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-semantics @bvsub)))

(define tests:bvmul
  (test-suite+
   "Tests for bvmul in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvmul-simplifications)
   (check-semantics @bvmul)))

(define tests:bvudiv
  (test-suite+
   "Tests for bvudiv in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvudiv-simplifications)
   (check-semantics @bvudiv)))

(define tests:bvsdiv
  (test-suite+
   "Tests for bvsdiv in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvsdiv-simplifications)
   (check-semantics @bvsdiv)))

(define tests:bvurem
  (test-suite+
   "Tests for bvurem in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvurem-simplifications)
   (check-semantics @bvurem)))

(define tests:bvsrem
  (test-suite+
   "Tests for bvsrem in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-bvsrem-simplifications)
   (check-semantics @bvsrem)))

(define tests:bvsmod
  (test-suite+
   "Tests for bvsmod in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-signed-remainder-simplifications @bvsmod)
   (check-semantics @bvsmod)))

(define tests:concat
  (test-suite+
   "Tests for concat in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-concat-simplifications)
   (check-concat-semantics)))

(define tests:extract
  (test-suite+
   "Tests for extract in rosette/base/bitvector.rkt"
   #:features '(qf_bv qf_lia)
   (check-extract-simplifications)
   (check-extract-semantics)
   (check-lifted-extract)))

(define tests:zero-extend
  (test-suite+
   "Tests for zero-extend in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-extend-simplifications @zero-extend)
   (check-extend-semantics @zero-extend)))

(define tests:sign-extend
  (test-suite+
   "Tests for sign-extend in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-extend-simplifications @sign-extend)
   (check-extend-semantics @sign-extend)))

(define tests:bitvector->integer
  (test-suite+
   "Tests for bitvector->integer in rosette/base/bitvector.rkt"
   #:features '(qf_bv qf_lia)
   (check-bv->*-semantics @bitvector->integer)
   (check-lifted-bv->*)))

(define tests:bitvector->natural
  (test-suite+
   "Tests for bitvector->natural in rosette/base/bitvector.rkt"
   #:features '(qf_bv qf_lia)
   (check-bv->*-semantics @bitvector->natural)))

(define tests:integer->bitvector
  (test-suite+
   "Tests for integer->bitvector in rosette/base/bitvector.rkt"
   #:features '(qf_bv qf_lia int2bv)
   (check-integer->bitvector-semantics)
   (check-lifted-integer->bitvector)))

(define tests:lifted-operators
  (test-suite+
   "Tests for lifted QF_BV operations in rosette/base/bitvector.rkt"
   #:features '(qf_bv)
   (check-lifted-bv-type)
   (check-lifted-unary)
   (check-lifted-binary)
   (check-lifted-nary)
   (check-lifted-concat)
   (check-lifted-extend)))

(module+ test
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
  (time (run-tests tests:bvsrem))
  (time (run-tests tests:bvsmod))
  (time (run-tests tests:concat))
  (time (run-tests tests:extract))
  (time (run-tests tests:zero-extend))
  (time (run-tests tests:sign-extend))
  (time (run-tests tests:bitvector->integer))
  (time (run-tests tests:bitvector->natural))
  (time (run-tests tests:integer->bitvector))
  (time (run-tests tests:lifted-operators))
  
  (solver-shutdown (solver)))
