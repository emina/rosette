#lang racket

(require rackunit rackunit/text-ui  "common.rkt" "solver.rkt"
         rosette/solver/solution 
         rosette/lib/roseunit 
         rosette/base/core/term rosette/base/core/bool
         rosette/base/core/real
         rosette/base/core/polymorphic rosette/base/core/merge 
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/equality @equal?)
         (only-in rosette evaluate))


(define-symbolic a b c d e f g @boolean?)
(define-symbolic xi yi zi @integer?)
(define-symbolic xr yr zr @real?)

(define minval -4)
(define maxval 4)
(define maxval+1 5)


(define-syntax-rule (check-valid? (op e ...) expected)
  (let ([actual (op e ...)])
    (check-equal? actual expected) 
    ;(printf "ASSERTS: ~a\n" (asserts))
    (define preconditions (asserts))
    (clear-asserts!)
    (check-pred unsat? (apply solve (! (@equal? (expression op e ...) expected)) preconditions))))

(define-syntax-rule (test-valid? ([var sym] ...) (op e ...) expected)
  (let ([actual (op e ...)])
    (check-equal? actual expected)
    (define preconditions (asserts))
    ;(printf "ASSERTS: ~a\n" (asserts))
    (clear-asserts!)
    (for* ([var (in-range minval maxval+1)] ...)
      (define sol (sat (make-immutable-hash (list (cons sym var) ...))))
      (and (for/and ([pre preconditions]) (evaluate pre sol))
           ;(printf "sol: ~a\n" sol)
           (check-true (evaluate (@equal? (expression op e ...) expected) sol))))))


(define-syntax-rule (check-cast (type val) (accepted? result))
  (with-handlers ([exn:fail? (lambda (e) (check-equal? accepted? #f))])  
    (let-values ([(actual-result asserts) (with-asserts (type-cast type val))])
      (check-equal? actual-result result)
      (match asserts
        [(list)   (check-equal? accepted? #t)]
        [(list v) (check-equal? accepted? v)]
        [_ (fail "found more than 1 assertion")]))))

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

(define-syntax check-num-exn
  (syntax-rules ()
    [(_ expr)
     (check-exn exn:fail? (thunk (with-asserts-only expr)))]
    [(_ pred expr)
     (check-exn pred (thunk (with-asserts-only expr)))]))

(define-syntax-rule (check-state actual expected-value expected-asserts)
  (let-values ([(v a) (with-asserts actual)])
    (check-equal? v expected-value)
    (check-equal? (apply set a) (apply set expected-asserts))))

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
  (check-cast (@real? (merge a 1 xr)) (#t (ite a 1 xr)))
  (check-cast (@real? (merge* (cons a xi) (cons b xr) (cons c '()))) 
              ((|| a b) (merge* (cons a (@integer->real xi)) (cons b xr))))
  (check-cast (@real? (merge a b '())) (#f (merge a b '()))))

(define (check-integer?)
  (check-equal? (@integer? 1) #t)
  (check-equal? (@integer? -1) #t)
  (check-equal? (@integer? -1.0001) #f)
  (check-equal? (@integer? xi) #t)
  (check-equal? (@integer? xr) (@int? xr))
  (check-equal? (@integer? (@real->integer xr)) #t)
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
              ((|| a (&& (! a) (@int? xr)))
               (merge* (cons a xi) (cons (&& (! a) (@int? xr)) (@real->integer xr)))))
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
  (check-valid? (@+ x 0.0) x)
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
  (check-valid? (@* 0.0 x) 0)
  (check-valid? (@* x 0.0) 0)
  (check-valid? (@* 1.0 x) x)
  (check-valid? (@* x 1.0) x)
  (check-valid? (@* -1.0 x) (@- x))
  (check-valid? (@* x -1.0) (@- x))
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
  (check-valid? (@* (@* x (@/ 1 y)) (@* (@/ 1 x) y z)) z)
  (check-valid? (@* (@* (@/ 1 x) (@/ 1 y)) (@* x y z)) z)
  (check-valid? (@* (@* x y) (@* (@/ 1.0 x) (@/ 1 y))) 1)
  (check-valid? (@* (@* x y) (@* (@/ 1.0 x) (@/ 1.0 y))) 1)
  (check-valid? (@* (@* x y) (@* (@/ 1 x) (@/ 1.0 y))) 1)
  )

(define (check-division-simplifications div x y z [epsilon 0])
  (check-valid? (div 0 x) 0)
  (check-valid? (div x 1) x)
  (check-valid? (div x -1) (@- x))
  (check-valid? (div 0.0 x) 0)
  (check-valid? (div x 1.0) x)
  (check-valid? (div x -1.0) (@- x))
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

(define (check-quotient-simplifications [x xi] [y yi] [z zi])
  ; We can't use Z3 to check all simplifications for quotient because 
  ; they are in the undecidable fragment and z3 either runs forever
  ; or returns 'unknown.
  (check-valid? (@quotient 0 x) 0)
  (check-valid? (@quotient x 1) x)
  (check-valid? (@quotient x -1) (@- x))
  (check-valid? (@quotient 0.0 x) 0)
  (check-valid? (@quotient x 1.0) x)
  (check-valid? (@quotient x -1.0) (@- x))
  (check-valid? (@quotient x x) 1)
  (check-valid? (@quotient x (@- x)) -1)
  (check-valid? (@quotient (@- x) x) -1)
  (check-valid? (@quotient (ite a 4 6) 2) 
                (ite a (@quotient 4 2) (@quotient 6 2)))
  (check-valid? (@quotient 12 (ite a 4 6)) 
                (ite a (@quotient 12 4) (@quotient 12 6)))
  (check-valid? (@quotient (@quotient x 4) 2) (@quotient x 8))
  (test-valid?  ([i x][j y][k z]) (@quotient (@* x y z) y) (@* x z)))

(define (check-remainder-simplifications op [x xi] [y yi] [z zi])
  (check-valid? (op 0 x) 0)
  (check-valid? (op x 1) 0)
  (check-valid? (op x -1) 0)
  (check-valid? (op 0.0 x) 0)
  (check-valid? (op x 1.0) 0)
  (check-valid? (op x -1.0) 0)
  (check-valid? (op x x) 0)
  (check-valid? (op x (@- x)) 0)
  (check-valid? (op (@- x) x) 0)
  (check-valid? (op (ite a 4 6) 3) 
                (ite a (op 4 3) (op 6 3)))
  (check-valid? (op 18 (ite a 4 6)) 
                (ite a (op 18 4) (op 18 6))))

(define (check-abs-simplifications x)
  (check-valid? (@abs (@abs x)) (@abs x)))

(define (check-int?-semantics) 
  (check-pred unsat? (solve (@= xr 1) (! (@int? xr))))
  (check-pred unsat? (solve (@= xr 1.0) (! (@int? xr))))
  (check-pred unsat? (solve (@= xr 1.2) (@int? xr)))
  (check-pred unsat? (solve (@= yr 4) (@= xr 2) 
                            (! (@int? (@/ yr xr)))))
  (check-pred unsat? (solve (@= yr 4) (@= xr 2) 
                            (@int? (@/ xr yr)))))

(define (check-integer->real-semantics)
  (check-pred unsat? (solve (@= xi 2) (@= 2.3 (@integer->real xi))))
  (check-pred unsat? (solve (@= xr 2) (! (@= 2.0 (@integer->real xr)))))
  (clear-asserts!)
  (check-num-exn #px"expected integer?" (@integer->real 2.3))
  (check-num-exn #px"expected integer?" (@integer->real 'a))
  (check-num-exn #px"expected integer?" (@integer->real (merge a "3" '())))
  (check-state (@integer->real 1) 1 (list))
  (check-state (@integer->real xi) (@integer->real xi) (list))
  (check-state (@integer->real (@+ xi yi)) (@integer->real (@+ xi yi)) (list))
  (check-state (@integer->real xr) (@integer->real (@real->integer xr)) (list (@int? xr)))
  (check-state (@integer->real (merge a 1 'a)) 1 (list a))
  (check-state (@integer->real (merge a xr 'a)) 
               (@integer->real (@real->integer xr))
               (list (&& a (@int? xr))))
  (check-state (@integer->real (merge a xi xr)) 
               (@integer->real (merge* (cons a xi)
                                       (cons (&& (! a) (@int? xr)) 
                                             (@real->integer xr))))
               (list (|| a (&& (! a) (@int? xr)))))
  (check-state (@integer->real (merge* (cons a xi) (cons b xr) (cons c 'a))) 
               (@integer->real (merge* (cons a xi)
                                       (cons (&& b (@int? xr)) 
                                             (@real->integer xr))))
               (list (|| a (&& b (@int? xr))))))

(define (check-real->integer-semantics) 
  (check-valid? (@real->integer (@integer->real xi)) xi)
  (check-valid? (@real->integer (ite a (@integer->real xi) (@integer->real yi))) 
                (merge a xi yi))
  (check-valid? (@real->integer (ite a xr (@integer->real yi))) 
                (merge a (@real->integer xr) yi))
  (check-valid? (@real->integer (ite a (@integer->real xi) yr)) 
                (merge a xi (@real->integer yr)))
  (check-pred unsat? (solve (@= xr 2.3) (@= xr (@real->integer xr))))
  (check-pred unsat? (solve (@= xr 2.0) (! (@= xr (@real->integer xr)))))
  (clear-asserts!)
  (check-num-exn #px"expected real?" (@real->integer 'a))
  (check-num-exn #px"expected real?" (@real->integer (merge a "3" '())))  
  (check-state (@real->integer 1) 1 (list))
  (check-state (@real->integer xi) xi (list))
  (check-state (@real->integer (@+ xi yi)) (@+ xi yi) (list))
  (check-state (@real->integer xr) (@real->integer xr) (list))  
  (check-state (@real->integer (merge a 1 'a)) 1 (list a))
  (check-state (@real->integer (merge a xr 'a)) 
               (@real->integer xr) (list a))
  (check-state (@real->integer (merge a xi xr)) 
               (@real->integer (merge a xi (@real->integer xr)))
               (list))
  (check-state (@real->integer (merge* (cons a xi) (cons b xr) (cons c 'a))) 
               (@real->integer (merge* (cons a (@integer->real xi)) (cons b xr)))
               (list (|| a b)))
  )

(define (check-lifted-unary)
  (check-num-exn (@abs 'a))
  (check-num-exn (@abs (merge a "3" '())))
  (check-state (@abs -3) 3 (list))
  (check-state (@abs -3.6) 3.6 (list))
  (check-state (@abs xi) (@abs xi) (list))
  (check-state (@abs xr) (@abs xr) (list))
  (check-state (@abs (merge a xi "3")) (@abs xi) (list a))
  (check-state (@abs (merge a xr "3")) (@abs xr) (list a))
  (check-state (@abs (merge a xi xr)) (merge a (@abs xi) (@abs xr)) (list))
  (check-state (@abs (merge* (cons a xi) (cons b xr) (cons c ""))) 
               (merge* (cons a (@abs xi)) (cons b (@abs xr))) (list (|| a b))))

(define (check-lifted-binary)
  (check-num-exn (@+ 1 'a))
  (check-num-exn (@+ 'a 1))
  (check-num-exn (@+ 1 (ite a 'a 'b)))
  ; int/int
  (check-state (@+ xi yi) (@+ xi yi) (list))
  (check-state (@+ (merge a xi #f) yi) (@+ xi yi) (list a))
  (check-state (@+ xi (merge a yi #f)) (@+ xi yi) (list a))
  (check-state (@+ (merge a xi #f) (merge b yi #f)) (@+ xi yi) (list a b))
  ; int/real
  (check-state (@+ xi yr) (@+ (@integer->real xi) yr) (list))
  (check-state (@+ (merge a xi #f) yr) (@+ (@integer->real xi) yr) (list a))
  (check-state (@+ xi (merge a yr #f)) (@+ (@integer->real xi) yr) (list a))
  (check-state (@+ (merge a xi #f) (merge b yr #f)) (@+ (@integer->real xi) yr) (list a b))
  ; int/union
  (check-state (@+ xi (merge a yi yr)) (@+ (@integer->real xi) (merge a (@integer->real yi) yr)) (list))
  (check-state (@+ (merge b xi #f) (merge a yi yr))
               (@+ (@integer->real xi) (merge a (@integer->real yi) yr))
               (list b))
  (check-state (@+ xi (merge* (cons a yi) (cons b yr) (cons c #f)))
               (@+ (@integer->real xi) (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list (|| a b)))
  (check-state (@+ (merge d xi #f)  (merge* (cons a yi) (cons b yr) (cons c #f)))
               (@+ (@integer->real xi) (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list d (|| a b)))
  ; real/int
  (check-state (@+ xr yi) (@+ xr (@integer->real yi)) (list))
  (check-state (@+ (merge a xr #f) yi) (@+ xr (@integer->real yi)) (list a))
  (check-state (@+ xr (merge a yi #f)) (@+ xr (@integer->real yi)) (list a))
  (check-state (@+ (merge a xr #f) (merge b yi #f)) (@+ xr (@integer->real yi)) (list a b))
  ; real/real
  (check-state (@+ xr yr) (@+ xr yr) (list))
  (check-state (@+ (merge a xr #f) yr) (@+ xr yr) (list a))
  (check-state (@+ xr (merge a yr #f)) (@+ xr yr) (list a))
  (check-state (@+ (merge a xr #f) (merge b yr #f)) (@+ xr yr) (list a b))
  ; real/union
  (check-state (@+ xr (merge a yi yr)) 
               (@+ xr (merge a (@integer->real yi) yr))
               (list))
  (check-state (@+ (merge b xr #f) (merge a yi yr)) 
               (@+ xr (merge a (@integer->real yi) yr))
               (list b))
  (check-state (@+ xr (merge* (cons a yi) (cons b yr) (cons c #f))) 
               (@+ xr (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list (|| a b)))
  (check-state (@+ (merge c xr #f) (merge* (cons a yi) (cons b yr) (cons d #f))) 
               (@+ xr (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list c (|| a b)))
  ; union/int
  (check-state (@+ (merge a yi yr) xi) (@+ (@integer->real xi) (merge a (@integer->real yi) yr)) (list))
  (check-state (@+ (merge a yi yr) (merge b xi #f))
               (@+ (@integer->real xi) (merge a (@integer->real yi) yr))
               (list b))
  (check-state (@+ (merge* (cons a yi) (cons b yr) (cons c #f)) xi)
               (@+ (@integer->real xi) (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list (|| a b)))
  (check-state (@+ (merge* (cons a yi) (cons b yr) (cons c #f)) (merge d xi #f))
               (@+ (@integer->real xi) (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list d (|| a b)))
  ; union/real
  (check-state (@+ (merge a yi yr) xr) 
               (@+ xr (merge a (@integer->real yi) yr))
               (list))
  (check-state (@+ (merge a yi yr) (merge b xr #f)) 
               (@+ xr (merge a (@integer->real yi) yr))
               (list b))
  (check-state (@+ (merge* (cons a yi) (cons b yr) (cons c #f)) xr) 
               (@+ xr (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list (|| a b)))
  (check-state (@+ (merge* (cons a yi) (cons b yr) (cons d #f)) (merge c xr #f)) 
               (@+ xr (merge* (cons a (@integer->real yi)) (cons b yr)))
               (list c (|| a b)))
  ; union/union 1
  (check-state (@+ (merge* (cons a xi) (cons b xr)) 
                   (merge* (cons c yi) (cons d yr)))
               (@+ (merge* (cons a (@integer->real xi)) (cons b xr))
                   (merge* (cons c (@integer->real yi)) (cons d yr)))
               (list))
  (check-state (@+ (merge* (cons a xi) (cons b xr) (cons e #f)) 
                   (merge* (cons c yi) (cons d yr)))
               (@+ (merge* (cons a (@integer->real xi)) (cons b xr))
                   (merge* (cons c (@integer->real yi)) (cons d yr)))
               (list (|| a b)))
  (check-state (@+ (merge* (cons a xi) (cons b xr)) 
                   (merge* (cons c yi) (cons d yr) (cons e #f)))
               (@+ (merge* (cons a (@integer->real xi)) (cons b xr))
                   (merge* (cons c (@integer->real yi)) (cons d yr)))
               (list (|| c d)))
  (check-state (@+ (merge* (cons a xi) (cons b xr) (cons e #f)) 
                   (merge* (cons c yi) (cons d yr) (cons f #f)))
               (@+ (merge* (cons a (@integer->real xi)) (cons b xr))
                   (merge* (cons c (@integer->real yi)) (cons d yr)))
               (list (|| a b) (|| c d))))

(define (check-lifted-nary)
  (check-num-exn (@+ 1 2 'a))
  ; ints
  (check-state (@+ xi 2 yi 1) (@+ 3 xi yi) (list))
  ; reals
  (check-state (@+ xr 2 yr 1) (@+ 3 xr yr) (list))
  ; at least one real primitive
  (check-state (@+ xi 2 yi 1 xr) (@+ 3 xr (@integer->real xi) (@integer->real yi)) (list))
  (check-state (@+ xi 2 yi 1.1) (@+ 3.1 (@integer->real xi) (@integer->real yi)) (list))
  (check-state (@+ 3 zr (merge a xi yr)) (@+ 3 zr (merge a (@integer->real xi) yr))
               (list))
  ; no real primitives
  (check-state (@+ xi 1 (merge a yi yr)) 
               (@+ (@integer->real xi) 1 (merge a (@integer->real yi) yr))
               (list))
  )

(define tests:real?
  (test-suite+
   "Tests for real? in rosette/base/real.rkt"
   (check-real?)
   (check-real-cast)))

(define tests:integer?
  (test-suite+
   "Tests for integer? in rosette/base/real.rkt"
   (check-integer?)
   (check-integer-cast)
   ))

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
   "Tests for + in rosette/base/real.rkt"
   (check-+-simplifications xi yi zi)
   (check-+-simplifications xr yr zr)
   (check-semantics @+ xi yi zi)
   (check-semantics @+ xr yr zr)))

(define tests:-
  (test-suite+
   "Tests for - in rosette/base/real.rkt"
   (check---simplifications xi yi zi)
   (check---simplifications xr yr zr)
   (check-semantics @- xi yi zi)
   (check-semantics @- xr yr zr)))

(define tests:*
  (test-suite+
   "Tests for * in rosette/base/real.rkt"
   (check-*-simplifications xi yi zi)
   (check-*-simplifications xr yr zr)
   (check-*-real-simplifications)
   (check-semantics @* xi yi zi)
   (check-semantics @* xr yr zr)))

(define tests:/
  (test-suite+
   "Tests for / in rosette/base/real.rkt"
   (check-division-simplifications @/ xr yr zr (/ 2 10))
   (check-semantics @/ xr yr zr (lambda (x) (not (zero? x))))
   ))

(define tests:quotient
  (test-suite+
   "Tests for quotient in rosette/base/real.rkt"
   (check-quotient-simplifications)
   (check-semantics @quotient xi yi zi (lambda (x) (not (zero? x))))))

(define tests:remainder
  (test-suite+
   "Tests for remainder in rosette/base/real.rkt"
   (check-remainder-simplifications @remainder)
   (check-semantics @remainder xi yi zi (lambda (x) (not (zero? x))))))

(define tests:modulo
  (test-suite+
   "Tests for modulo in rosette/base/real.rkt"
   (check-remainder-simplifications @modulo)
   (check-semantics @modulo xi yi zi (lambda (x) (not (zero? x))))))

(define tests:abs
  (test-suite+
   "Tests for abs in rosette/base/real.rkt"
   (check-abs-simplifications xi)
   (check-abs-simplifications xr)
   (check-semantics @abs xi yi zi)
   (check-semantics @abs xr yr zr)))

(define tests:int?
  (test-suite+
   "Tests for int? in rosette/base/real.rkt"
   (check-int?-semantics)))

(define tests:integer->real
  (test-suite+
   "Tests for integer->real in rosette/base/real.rkt"
   (check-integer->real-semantics)))

(define tests:real->integer
  (test-suite+
   "Tests for real->integer in rosette/base/real.rkt"
   (check-real->integer-semantics)))

(define tests:lifted
  (test-suite+
   "Tests for lifted operators in rosette/base/real.rkt"
   (check-lifted-unary)
   (check-lifted-binary)
   (check-lifted-nary)
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
(time (run-tests tests:quotient))
(time (run-tests tests:remainder))
(time (run-tests tests:modulo))
(time (run-tests tests:abs))
(time (run-tests tests:int?))
(time (run-tests tests:integer->real))
(time (run-tests tests:real->integer))
(time (run-tests tests:lifted))

(solver-shutdown (solver))
