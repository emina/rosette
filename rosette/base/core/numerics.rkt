#lang racket

(require "op.rkt" "term.rkt" "polymorphic.rkt" 
         "real.rkt" "bool.rkt" "bitwise.rkt"
         "merge.rkt" "safe.rkt")

(provide @number? @positive? @negative? @zero? @even? @odd?
         @add1 @sub1 @sgn @truncate @floor @ceiling @min @max
         @exact->inexact @inexact->exact @expt @sqrt
         @bitwise-not @bitwise-and @bitwise-ior @bitwise-xor
         @<< @>> @bitwise-bit-set? @bitwise-bit-field)

(define (@number? v)   (or (number? v) (@real? v)))
(define (@positive? x) (@> x 0))
(define (@negative? x) (@< x 0))
(define (@zero? x)     (@= x 0))
(define (@even? x)     (@zero? (@remainder x 2)))
(define (@odd? x)      (! (@even? x)))
(define (@add1 x)      (@+ x 1))
(define (@sub1 x)      (@- x 1))

(define (@sgn x)
  (if (number? x) 
      (sgn x)
      (merge* (cons (@positive? x) 1)
              (cons (@negative? x) -1)
              (cons (@zero? x) 0))))

(define ($truncate x)
  (match x
    [(? real?) (truncate x)]
    [(term _ (== @integer?)) x]
    [(term _ (== @real?)) 
     (let ([xi (@real->integer x)])
       (merge (@< x 0) (@+ xi 1) xi))]))

(define @truncate (lift-op $truncate))

(define (@floor x) (@real->integer x))

(define ($ceiling x)
  (match x
    [(? real?) (ceiling x)]
    [(term _ (== @integer?)) x]
    [(term _ (== @real?))
     (let* ([xi (@real->integer x)])
       (merge (@<= x xi) xi (@+ xi 1)))]))

(define @ceiling (lift-op $ceiling))

(define extreme 
  (case-lambda 
    [(op x) x]
    [(op x y) (merge (op x y) x y)]
    [(op x y . z) (apply extreme op (extreme op x y) z)]))

(define @min (lift-op (curry extreme @<=)))
(define @max (lift-op (curry extreme @>=)))

(define (@exact->inexact x)
  (if (number? x) 
      (exact->inexact x)
      (numeric-coerce x 'exact->inexact)))

(define (@inexact->exact x)
  (if (number? x) 
      (inexact->exact x)
      (numeric-coerce x 'inexact->exact)))

(define (@expt z w)
  (match* (z w)
    [((? number?) (? number?)) (expt z w)]
    [(_ 0) 
     (assert (@real? z) (arguments-error 'expt "expected a number?" "z" z))
     1]
    [(_ (? integer?))
     (if (positive? w) 
         (apply @* (make-list w z))
         (@/ 1 (@* (make-list (- w) z))))]
    [(_ _) (expt z w)]))

(define (non-negative-literal? x) (and (real? x) (>= x 0)))

(define ($sqrt x)
  (match x
    [(? number?) (sqrt x)]
    [(expression (== @*) y y) (@abs y)]
    [(expression (== @*) (? non-negative-literal? a) y y) (@* (sqrt a) (@abs y))]
    [(expression (== @*) y zs ...)
     (let ([len (length zs)])
       (if (and (odd? len) (for/and ([z zs]) (equal? y z)))
           (apply @* (make-list (/ (+ len 1) 2) (@abs y)))
           (expression @sqrt x)))]
    [(expression (== @*) (? non-negative-literal? a) y zs ...)
     (let ([len (length zs)])
       (if (and (odd? len) (for/and ([z zs]) (equal? y z)))
           (apply @* (sqrt a) (make-list (/ (+ len 1) 2) (@abs y)))
           (expression @sqrt x)))]
    [(expression (== ite) a (? non-negative-literal? b) (? non-negative-literal? c))
     (merge a (sqrt b) (sqrt c))]
    [_ (expression @sqrt x)]))
    
(define-operator @sqrt
  #:name 'sqrt
  #:type T*->real?
  #:unsafe $sqrt
  #:safe (lambda (x)
           (let ([y (coerce x @real? 'sqrt)])
             (assert (@<= 0 y) 
                     (argument-error 'sqrt "expected a non-negative real?" x))
             ($sqrt x))))

(define-syntax-rule (define-lifted-bitwise-operator @op $op op)
  (define-operator @op
    #:name 'op
    #:type T*->integer?
    #:unsafe $op
    #:safe (lift-int-op $op 'op)))

(define (lift-int-op op caller)
  (case (procedure-arity op)
    [(1)  (lambda (x) (op (coerce x @integer? caller)))]
    [(2)  (lambda (x y) (op (coerce x @integer? caller) (coerce y @integer? caller)))]
    [else (case-lambda [() (op)]
                       [(x) (op (coerce x @integer? caller))]
                       [(x y) (op (coerce x @integer? caller) (coerce y @integer? caller))]
                       [xs (apply op (for/list ([x xs]) (coerce x @integer? caller)))])]))
               
(define ($bitwise-not x)
  (match x
    [(? integer?) (bitwise-not x)]
    [(expression (== @bitwise-not) y) y]
    [_ (expression @bitwise-not x)]))

(define $bitwise-and (bitwise-connective bitwise-and $bitwise-and @bitwise-and @bitwise-ior -1 0))
(define $bitwise-ior (bitwise-connective bitwise-ior $bitwise-ior @bitwise-ior @bitwise-and 1 -0))

(define $bitwise-xor
  (case-lambda
    [() 0]
    [(x) x]
    [(x y) (or (simplify-xor x y) 
               (sort/expression @bitwise-xor x y))]
    [xs (let*-values ([(lits terms) (partition integer? xs)]
                      [(lit) (apply bitwise-xor lits)])
          (if (null? terms)
              lit
              (match (simplify* (if (null? lits) terms (cons lit terms)))
                [(list y) y]
                [(list a ... (? integer? b) c ...) 
                 (apply expression @bitwise-xor b (sort (append a c) term<?))]
                [ys (apply expression @bitwise-xor (sort ys term<?))])))]))

(define ($<< x y)
  (match* ((finitize x) (finitize y))
    [(x 0) x]
    [(0 _) 0]
    [((? integer? x) (? integer? y)) 
     (if (> y 0) (finitize (arithmetic-shift x y)) 0)]
    [(x y) (expression @<< x y)]))
 
(define ($>> x y)
  (match* ((finitize x) (finitize y))    
    [(x 0) x]
    [(0 _) 0]
    [(-1 _) -1]
    [((? integer? x) (? integer? y)) 
     (cond [(> y 0) (finitize (arithmetic-shift x (- y)))]
           [(> x 0) 0]
           [else -1])]
    [(x y) (expression @>> x y)]))


(define-lifted-bitwise-operator @bitwise-not $bitwise-not bitwise-not)
(define-lifted-bitwise-operator @bitwise-and $bitwise-and bitwise-and)
(define-lifted-bitwise-operator @bitwise-ior $bitwise-ior bitwise-ior)
(define-lifted-bitwise-operator @bitwise-xor $bitwise-xor bitwise-xor)
(define-lifted-bitwise-operator @<< $<< <<)
(define-lifted-bitwise-operator @>> $>> >>)

(define (@bitwise-bit-set? n m)
  (if (and (integer? n) (integer? m))
      (bitwise-bit-set? n m)
      (! (@= 0 (@bitwise-and n (@<< 1 m))))))

(define (@bitwise-bit-field n start end)
  (if (and (integer? n) (integer? start) (integer? end))
      (bitwise-bit-field n start end)
      (let ([n (coerce n @integer? 'bitwise-bit-field)]
            [start (coerce start @integer? 'bitwise-bit-field)]
            [end (coerce end @integer? 'bitwise-bit-field)])
        (assert (@>= start 0) (arguments-error 'bitwise-bit-field "expected start >= 0"))
        (assert (@>= end start) (arguments-error 'bitwise-bit-field "expected start >= end"))
        (@bitwise-and (@sub1 (@<< 1 (@- end start))) (@>> n start)))))

; Partial evaluation rules for connectives (bitwise-and and bitwise-or).  
; The terms iden and !iden should be numeric literals.
(define-syntax-rule (bitwise-connective op $op @op @co iden !iden)
  (case-lambda 
    [() iden]
    [(x) x]
    [(x y) 
     (match* (x y)
       [((? integer?) (? integer?)) (op x y)]
       [(iden _) y]
       [(_ iden) x]
       [(!iden _) !iden]
       [(_ !iden) !iden]
       [(_ _)
        (or
         (simplify-connective @op @co @bitwise-not integer? !iden x y)
         (sort/expression @op x y))])]
    [xs
     (let*-values ([(lits terms) (partition integer? xs)]
                   [(lit) (apply op lits)])
       (if (or (= lit !iden) (null? terms)) 
           lit
           (match (simplify-connective* @op @co @bitwise-not integer? !iden (remove-duplicates terms))
             [(list (? integer? u)) (op lit u)]
             [(list y) ($op lit y)]
             [ys (if (= lit iden)
                     (apply expression @op (sort ys term<?))
                     (apply expression @op lit (sort ys term<?)))])))]))

; Simplification rulues for bitwise-xor.
(define (simplify-xor x y)
  (match* (x y)
    [((? integer?) (? integer?)) (bitwise-xor x y)]
    [(_ (== x)) 0]
    [(_ 0) x]
    [(0 _) y]
    [(_ -1) (@bitwise-not x)]
    [(-1 _) (@bitwise-not y)]
    [(_ (expression (== @bitwise-not) (== x))) -1]
    [((expression (== @bitwise-not) (== y)) _) -1]
    [(_ _) #f]))
