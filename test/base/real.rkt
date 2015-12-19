#lang racket

(require rackunit rackunit/text-ui  "common.rkt"
         rosette/solver/smt/z3  rosette/solver/solution 
         rosette/lib/util/roseunit 
         rosette/base/core/term rosette/base/core/bool
         rosette/base/core/real
         rosette/base/core/polymorphic rosette/base/core/merge 
         rosette/base/core/assert
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/equality @equal?))

(define solver (new z3%))
(finite-number-semantics? #f)

(define-symbolic a b c d e f g @boolean?)
(define-symbolic xi yi zi @integer?)
(define-symbolic xr yr zr @real?)

(define minval -4)
(define maxval 4)
(define maxval+1 5)

(define (solve  . asserts)
  (send/apply solver assert asserts)
  (begin0
    (send solver solve)
    (send solver clear)))

(define-syntax-rule (check-valid? (op e ...) expected)
  (let ([actual (op e ...)])
    (check-equal? actual expected) 
    ;(printf "ASSERTS: ~a\n" (asserts))
    (define preconditions (asserts))
    (clear-asserts)
    (check-pred unsat? (apply solve (! (@equal? (expression op e ...) expected)) preconditions))))

(define-syntax-rule (check-cast (type val) (accepted? result))
  (let-values ([(actual-accepted? actual-result) (cast type val)])
    (check-equal? actual-accepted? accepted?)
    (check-equal? actual-result result)))

(define (check-cmp-semantics op x y)
  (for* ([i (in-range minval maxval+1)]
         [j (in-range minval maxval+1)])
    (define actual (op i j))
    (define expected 
      ((solve (@= i x)
              (@= j y)
              (@equal? a (op x y))) a))
    (check-equal? actual expected)))

(define (check-semantics op x y z [right? (const #t)])
  (case (procedure-arity op)
    [(1) 
     (for ([i (in-range minval maxval+1)])
       (define actual (op i))
       (define expected 
         ((solve (@= i x)
                 (@= y (op x))) y))
       (check-= actual expected 0))]
    [else
     (for* ([i (in-range minval maxval+1)]
            [j (in-range minval maxval+1)] #:when (right? j))
       (define actual (op i j))
       ;(printf "(~a ~a ~a) = ~a\n" op i j actual)
       (define expected 
         ((solve (@= i x)
                 (@= j y)
                 (@= z (op x y))) z))
       (check-= actual expected 0))]))

(define (check-real?)
  (check-equal? (@real? 1) #t)
  (check-equal? (@real? -1.0001) #t)
  (check-equal? (@real? xi) #t)
  (check-equal? (@real? xr) #t)
  (check-equal? (@real? (merge a xi '())) a)
  (check-equal? (@real? a) #f)
  (check-equal? (@real? (merge a b '())) #f))

(define (check-real-cast)
  (check-cast (@real? 1) (#t 1))
  (check-cast (@real? -1.0001) (#t -1.0001))
  (check-cast (@real? xr) (#t xr))
  (check-cast (@real? xi) (#t (@integer->real xi)))
  (check-cast (@real? (merge a xi '())) (a (@integer->real xi)))
  (check-cast (@real? (merge a xr '())) (a xr))
  (check-cast (@real? (merge a xi xr)) (#t (merge a (@integer->real xi) xr)))
  (check-cast (@real? (merge* (cons a xi) (cons b xr) (cons c '()))) 
              ((|| a b) (merge* (cons a (@integer->real xi)) (cons b xr))))
  (check-cast (@real? (merge a b '())) (#f (merge a b '()))))

(define (check-integer?)
  (check-equal? (@integer? 1) #t)
  (check-equal? (@integer? -1) #t)
  (check-equal? (@integer? -1.0001) #f)
  (check-equal? (@integer? xi) #t)
  (check-equal? (@integer? xr) (@int? xr))
  (check-equal? (@integer? (merge a xi '())) a)
  (check-equal? (@integer? (merge a xr '())) (&& a (@int? xr)))
  (check-equal? (@integer? (merge a xr xi)) (|| (! a) (&& a (@int? xr))))
  (check-equal? (@integer? a) #f)
  (check-equal? (@integer? (merge a b '())) #f))

(define (check-integer-cast)
  (check-cast (@integer? 1) (#t 1))
  (check-cast (@integer? -1.0001) (#f -1.0001))
  (check-cast (@integer? xi) (#t xi))
  (check-cast (@integer? xr) ((@int? xr) (@real->integer xr)))
  (check-cast (@integer? (merge a xi '())) (a xi))
  (check-cast (@integer? (merge a xr '())) ((&& a (@int? xr)) (@real->integer xr)))
  (check-cast (@integer? (merge a xi xr)) 
              (#t (merge* (cons a xi) (cons (&& (! a) (@int? xr)) (@real->integer xr)))))
  (check-cast (@integer? (merge* (cons a xi) (cons b xr) (cons c '()))) 
              ((|| a (&& b (@int? xr))) 
               (merge* (cons a xi) (cons (&& b (@int? xr)) (@real->integer xr)))))
  (check-cast (@integer? (merge a b '())) (#f (merge a b '()))))

(define (check-=-simplifications)
  (check-valid? (@= 2 2.0) #t)
  (check-valid? (@= 2.1 2.0) #f)
  (check-valid? (@= (ite b 2 0) 2) b)
  (check-valid? (@= (ite b 2 0) 0) (! b))
  (check-valid? (@= (ite b 2 0) 1) #f)
  (check-valid? (@= 2 (ite b 2 0)) b)
  (check-valid? (@= 0 (ite b 2 0)) (! b))
  (check-valid? (@= 1 (ite b 2 0)) #f)
  (check-valid? (@= (ite a 2 2) (ite b 2 2)) #t)
  (check-valid? (@= (ite a 2 3) (ite b 4 5)) #f)
  (check-valid? (@= (ite a 2 3) (ite b 2 5)) (&& a b))
  (check-valid? (@= (ite a 2 3) (ite b 5 2)) (&& a (! b)))
  (check-valid? (@= (ite a 2 3) (ite b 3 5)) (&& (! a) b))
  (check-valid? (@= (ite a 2 3) (ite b 5 3)) (&& (! a) (! b))))

(define (check-cmp-simplifications l* g*)
  (check-equal? (g* xi yi) (l* yi xi))
  (check-equal? (g* xr yr) (l* yr xr))
  (check-valid? (l* (ite b 2 0) 1) (! b))
  (check-valid? (l* (ite b 1 3) 2) b)
  (check-valid? (l* (ite b 2 1) 3) #t)
  (check-valid? (l* (ite b 2 1) 0) #f)
  (check-valid? (l* 1 (ite b 2 0)) b)
  (check-valid? (l* 1 (ite b 0 3)) (! b))
  (check-valid? (l* 3(ite b 2 1)) #f)
  (check-valid? (l* 0 (ite b 2 1)) #t)
  (check-valid? (l* (ite a 2 1) (ite b 3 4)) #t)
  (check-valid? (l* (ite b 3 4) (ite a 2 1)) #f)
  (check-valid? (l* (ite a 3 1) (ite b 0 2)) (&& (! a) (! b)))
  (check-valid? (l* (ite a 1 3) (ite b 2 0)) (&& a b))
  (check-valid? (l* (ite a 1 3) (ite b 0 2)) (&& a (! b)))
  (check-valid? (l* (ite a 3 1) (ite b 2 0)) (&& (! a) b)))

(define (check-+-simplifications x y z)
  (check-nary @+ 0 x y z)
  (check-valid? (@+ x 1) (@+ 1 x))
  (check-valid? (@+ (@- x) x) 0)
  (check-valid? (@+ (@- (@+ x z)) x) (@- z))
  (check-valid? (@+ (@+ (@+ (@- x) z)) x) z)
  (check-valid? (@+ (@+ y 1) 1) (@+ y 2))
  (check-valid? (@+ (ite a 1 2) 3) (ite a 4 5))
  (check-valid? (@+ (@* 3 x) x) (@* 4 x))
  (check-valid? (@+ (@* 3 x) (@* -2 x)) x)
  (check-valid? (@+ (@+ x y) (@- x)) y)
  (check-valid? (@+ (@+ x y) (@- y)) x)
  (check-valid? (@+ (@+ -1 x y) (@+ 1 (@- x) (@- y))) 0)
  (check-valid? (@+ (@+ -1 (@- x) y) (@+ 1 x (@- y))) 0)
  (check-valid? (@+ (@+ -1 x y) (@+ 1 (@- x) (@- y) z)) z)
  (check-valid? (@+ (@+ x y) (@+ 1 (@- x) (@- y))) 1)
  (check-valid? (@+ (@+ -1 (@* 2 x) y) (@+ 1 (@* -2 x) (@- y) z)) z))

(define (check---simplifications x y z)
  (check-valid? (@- (@- x)) x)
  (check-valid? (@- (@* 3 x)) (@* -3 x))
  (check-valid? (@- x y) (@+ x (@- y))))

(define (check-*-simplifications x y z)
  (check-nary @* 1 x y z)
  (check-valid? (@* 0 x) 0)
  (check-valid? (@* x 0) 0)
  (check-valid? (@* 1 x) x)
  (check-valid? (@* x 1) x)
  (check-valid? (@* -1 x) (@- x))
  (check-valid? (@* x -1) (@- x))
  (check-valid? (@* (ite a 1 2) 3) (ite a 3 6))
  (check-valid? (@* (@* 3 x) 4) (@* 12 x))
  )

(define (check-*-real-simplifications [x xr] [y yr] [z zr])
  (check-valid? (@* (@/ x y) y) x)
  (check-valid? (@* (@/ x (@* y z)) y) (@/ x z))
  (check-valid? (@* (@/ 3 x) 4) (@/ (@* 3 4) x))
  (check-valid? (@* (@* (@/ x y) z) y) (@* x z))
  (check-valid? (@* (@* x y) (@/ z x)) (@* y z))
  (check-valid? (@* (@* x y) (@/ z y)) (@* x z))
  (check-valid? (@* (@* x y) (@* (@/ 1 x) (@/ 1 y))) 1)
  (check-valid? (@* (@* (@/ 1 x) y) (@* x (@/ 1 y))) 1)
  (check-valid? (@* (@* x (@/ 1 y)) (@* (@/ 1 x) y)) 1)
  (check-valid? (@* (@* (@/ 1 x) (@/ 1 y)) (@* x y)) 1)
  (check-valid? (@* (@* x y) (@* (@/ 1 x) (@/ 1 y) z)) z)
  ; The following test triggers Racket 6.1 pattern-matching bug!
  ; See comment in cancel* in rosette/base/core/real
  (check-valid? (@* (@* (@/ 1 x) y) (@* x (@/ 1 y) z)) z) 
  ;
  (check-valid? (@* (@* x (@/ 1 y)) (@* (@/ 1 x) y z)) z)
  (check-valid? (@* (@* (@/ 1 x) (@/ 1 y)) (@* x y z)) z))

(define (check-division-simplifications div x y z [epsilon 0])
  (check-valid? (div 0 x) 0)
  (check-valid? (div x 1) x)
  (check-valid? (div x -1) (@- x))
  (check-valid? (div x x) 1)
  (check-valid? (div x (@- x)) -1)
  (check-valid? (div (@- x) x) -1)
  (check-valid? (div (ite a (+ 4 epsilon) (+ 6 epsilon)) 2) 
                (ite a (div (+ 4 epsilon) 2) (div (+ 6 epsilon) 2)))
  (check-valid? (div 12 (ite a (+ 4 epsilon) (+ 6 epsilon))) 
                (ite a (div 12 (+ 4 epsilon)) (div 12 (+ 6 epsilon))))
  (check-valid? (div (div x 4) 2) (div x 8))
  (check-valid? (div (@* x y z) y) (@* x z))
  (check-valid? (div (@* 2 x y z) y) (@* 2 x z))
  (check-valid? (div (@* x y z) (@* y x)) z))

(define tests:real?
  (test-suite+
   "Tests for real? in rosette/base/real.rkt"
   (check-real?)
   (check-real-cast)))

(define tests:integer?
  (test-suite+
   "Tests for integer? in rosette/base/real.rkt"
   (check-integer?)
   (check-integer-cast)))

(define tests:=
  (test-suite+
   "Tests for = in rosette/base/real.rkt"
   (check-=-simplifications)
   (check-cmp-semantics @= xi yi)
   (check-cmp-semantics @= xr yr)))

(define tests:<
  (test-suite+
   "Tests for < in rosette/base/real.rkt"
   (check-cmp-simplifications @< @>)
   (check-cmp-semantics @< xi yi)
   (check-cmp-semantics @< xr yr)))

(define tests:<=
  (test-suite+
   "Tests for <= in rosette/base/real.rkt"
   (check-cmp-simplifications @<= @>=)
   (check-cmp-semantics @<= xi yi)
   (check-cmp-semantics @<= xr yr)))

(define tests:+
  (test-suite+
   "Tests for + in rosette/base/bitvector.rkt"
   (check-+-simplifications xi yi zi)
   (check-+-simplifications xr yr zr)
   (check-semantics @+ xi yi zi)
   (check-semantics @+ xr yr zr)))

(define tests:-
  (test-suite+
   "Tests for - in rosette/base/bitvector.rkt"
   (check---simplifications xi yi zi)
   (check---simplifications xr yr zr)
   (check-semantics @- xi yi zi)
   (check-semantics @- xr yr zr)))

(define tests:*
  (test-suite+
   "Tests for * in rosette/base/bitvector.rkt"
   (check-*-simplifications xi yi zi)
   (check-*-simplifications xr yr zr)
   (check-*-real-simplifications)
   (check-semantics @* xi yi zi)
   (check-semantics @* xr yr zr)))

(define tests:/
  (test-suite+
   "Tests for / in rosette/base/bitvector.rkt"
   (check-division-simplifications @/ xr yr zr (/ 2 10))
   (check-semantics @/ xr yr zr (lambda (x) (not (zero? x))))
   ))

(time (run-tests tests:real?))
(time (run-tests tests:integer?))
(time (run-tests tests:=))
(time (run-tests tests:<))
(time (run-tests tests:<=))
(time (run-tests tests:+))
(time (run-tests tests:-))
(time (run-tests tests:*))
(time (run-tests tests:/))

(finite-number-semantics? #t)
(send solver shutdown)