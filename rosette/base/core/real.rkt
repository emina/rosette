#lang racket

(require (for-syntax racket/syntax) racket/stxparam racket/stxparam-exptime)
(require "term.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" 
         "merge.rkt" "safe.rkt" "lift.rkt" "forall.rkt")

(provide @integer? @real? @= @< @<= @>= @> @+ @* @- @/ @quotient @remainder @modulo @abs
         @integer->real @real->integer @int?
         lift-op numeric-coerce T*->integer? T*->real?)

;; ----------------- Integer and Real Types ----------------- ;; 

(define (int? v)
  (match v
    [(? integer?) #t]
    [(term _ (== @integer?)) #t]
    [(term _ (== @real?)) (expression @int? v)]
    [(union xs (or (== @real?) (== @any/c)))
     (let-values ([(i r) (guarded-numbers xs)])
       (match* (i r)
         [((cons g _) #f) g]
         [(#f (cons g x)) (&& g (int? x))]
         [((cons gi _) (cons gr xr)) (|| gi (&& gr (int? xr)))]
         [(_ _) #f]))]
    [_ #f]))
   
(define-lifted-type @real?
  #:base real?
  #:is-a? (instance-of? real? @real?)
  #:methods
  [(define (least-common-supertype self t)
     (if (or (equal? self t) (equal? @integer? t)) self @any/c))
   (define (solvable-default self) 0)
   (define (type-eq? self u v) ($= u v)) 
   (define (type-equal? self u v) ($= u v))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? real?) v]
       [(term _ (== self)) v]
       [(term _ (== @integer?)) (integer->real v)]
       [(union xs (or (== @real?) (== @any/c)))
        (let-values ([(i r) (guarded-numbers xs)])
          (match* (i r)
            [((cons g x) #f)
             (assert g (numeric-type-error caller @real? v))
             (integer->real x)]
            [(#f (cons g x))
             (assert g (numeric-type-error caller @real? v))
             x]
            [((cons gi xi) (cons gr _))
             (unless (= (length xs) 2)
               (assert (|| gi gr) (numeric-type-error caller @real? v)))
             (ite* (cons gi (integer->real xi)) r)]
            [(_ _)
             (assert #f (numeric-type-error caller @real? v))]))]
       [_ (assert #f (numeric-type-error caller @real? v))]))
   (define (type-compress self force? ps) (generic-merge* ps))])
  
(define-lifted-type @integer?
  #:base integer?
  #:is-a? int?
  #:methods 
  [(define (least-common-supertype self t)
     (if (or (equal? self t) (equal? @real? t)) t @any/c))
   (define (solvable-default self) 0)
   (define (type-eq? self u v) ($= u v)) 
   (define (type-equal? self u v) ($= u v))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? integer?) v]
       [(term _ (== self)) v]
       [(term _ (== @real?))
        (assert (int? v) (numeric-type-error caller @integer? v))
        (real->integer v)]
       [(union xs (or (== @real?) (== @any/c)))
        (let-values ([(i r) (guarded-numbers xs)])
          (match* (i r)
            [((cons g x) #f)
             (assert g (numeric-type-error caller @integer? v))
             x]
            [(#f (cons g x))
             (assert (&& g (int? x)) (numeric-type-error caller @integer? v))
             (real->integer x)]
            [((cons gi xi) (cons gr xr))
             (let ([gr (&& (int? xr) gr)])
               (assert (|| gi gr) (numeric-type-error caller @integer? v))
               (merge* i (cons gr (real->integer xr))))]
            [(_ _) (assert #f (numeric-type-error caller @integer? v))]))]
       [_ (assert #f (numeric-type-error caller @integer? v))]))
   (define (type-compress self force? ps) (generic-merge* ps))])

;; ----------------- Lifting Utilities ----------------- ;; 
(define (guarded-numbers xs)
  (for/fold ([i #f][r #f]) ([gx xs])
    (match (cdr gx)
      [(or (? integer?) (term _ (== @integer?))) (values gx r)]
      [(or (? real?) (term _ (== @real?))) (values i gx)]
      [_ (values i r)])))

(define-match-expander ≈
  (lambda (stx)
    (syntax-case stx ()
      [(_ v) #`(or v #,(exact->inexact (syntax->datum #'v)))]))) 

(define (numeric-coerce v [caller 'numeric-coerce])
  (match v 
    [(? real?) v]
    [(term _ (or (== @integer?) (== @real?))) v]
    [(union xs (or (== @real?) (== @any/c)))
     (let-values ([(i r) (guarded-numbers xs)])
       (match* (i r)
         [((cons g x) #f) 
          (assert g (numeric-type-error caller @real? v)) 
          x]
         [(#f (cons g x)) 
          (assert g (numeric-type-error caller @real? v)) 
          x]
         [((cons gi _) (cons gr _))
          (cond [(= (length xs) 2) v]
                [else (assert (|| gi gr) (numeric-type-error caller @real? v)) 
                      (merge* i r)])]
         [(_ _) (assert #f (numeric-type-error caller @real? v))]))]
    [_ (assert #f (numeric-type-error caller @real? v))]))

(define (numeric-type-error name t . args)
  (arguments-error name (format "expected ~a arguments" t) "arguments" args))
 
(define (safe-apply-1 op x)
  (match (numeric-coerce x (object-name op))
    [(union (list (cons ga a) (cons gb b))) 
     (merge* (cons ga (op a)) (cons gb (op b)))]
    [a (op a)]))

(define (int-primitive? v)
  (or (integer? v) (and (term? v) (equal? (get-type v) @integer?))))

(define (real-primitive? v)
  (or (real? v) (and (term? v) (equal? (get-type v) @real?))))

(define (safe-apply-2 op x y)
  (define caller (object-name op))
  (define a (numeric-coerce x caller))
  (define b (numeric-coerce y caller))  
  (match* (a b)
    [((? int-primitive?)(? int-primitive?)) (op a b)]
    [((? real-primitive?)(? real-primitive?)) (op a b)]
    [(_ _) (op (type-cast @real? a caller) (type-cast @real? b caller))]))
                            
(define (safe-apply-n op xs) 
  (define caller (object-name op))
  (define ys (for/list ([x xs]) (numeric-coerce x caller)))
  (match ys
    [(or (list (? int-primitive?) ...) (list (? real-primitive?) ...)) (apply op ys)]
    [_ (apply op (for/list ([y ys]) (type-cast @real? y caller)))]))
     
(define (lift-op op)
  (case (procedure-arity op)
    [(1)  (lambda (x) (safe-apply-1 op x))]
    [(2)  (lambda (x y) (safe-apply-2 op x y))]
    [else (case-lambda [() (op)]
                       [(x) (safe-apply-1 op x)]
                       [(x y) (safe-apply-2 op x y)]
                       [xs (safe-apply-n op xs)])]))

(define-syntax-rule (define-lifted-operator @op $op type)
  (define-operator @op
    #:identifier (string->symbol (substring (symbol->string '@op) 1))
    #:range type
    #:unsafe $op
    #:safe (lift-op $op)))
    
;; ----------------- Predicates ----------------- ;; 

(define-operator @int?
    #:identifier 'int?
    #:range T*->boolean?
    #:unsafe int?
    #:safe int?)

(define $=  (compare @= $= = sort/expression #t))
(define $<= (compare @<= $<= <= expression #t))
(define $<  (compare @< $< < expression #f))
(define $>= (case-lambda [(x y) ($<= y x)] [xs (apply $<= (reverse xs))]))
(define $>  (case-lambda [(x y) ($< y x)] [xs (apply $< (reverse xs))]))

(define-syntax-rule (compare @op $op op expr same=true?)
  (case-lambda 
    [(x y)
     (match* (x y)
       [((? real?) (? real?)) (op x y)]
       [(_ (== x)) same=true?]
       [((expression (== ite) a (? real? b) (? real? c)) (? real? d)) (merge a (op b d) (op c d))]
       [((? real? d) (expression (== ite) a (? real? b) (? real? c))) (merge a (op d b) (op d c))]
       [((expression (== ite) a (? real? b) (? real? c)) 
         (expression (== ite) d (? real? e) (? real? f)))
        (let ([b~e (op b e)] 
              [b~f (op b f)] 
              [c~e (op c e)] 
              [c~f (op c f)])
          (or (and b~e b~f c~e c~f)
              (|| (&& a d b~e) (&& a (! d) b~f) (&& (! a) d c~e) (&& (! a) (! d) c~f))))]
       [(a (expression (== @+) (? real? r) a)) (op 0 r)]
       [((expression (== @+) (? real? r) a) a) (op r 0)]
       [(_ _) (expr @op x y)])]
    [(x y . zs) 
     (apply && ($op x y) (for/list ([a (in-sequences (in-value y) zs)][b zs]) ($op a b)))]))

(define-lifted-operator @=  $= T*->boolean?)
(define-lifted-operator @<= $<= T*->boolean?)
(define-lifted-operator @>= $>= T*->boolean?)
(define-lifted-operator @<  $< T*->boolean?)
(define-lifted-operator @>  $> T*->boolean?)


;; ----------------- Int and Real Operators ----------------- ;; 

(define $+
  (case-lambda
    [() 0]
    [(x) x]
    [(x y) (or (simplify-+ x y) (sort/expression @+ x y))]
    [xs 
     (let*-values ([(lits terms) (partition real? xs)]
                   [(lit) (apply + lits)])
       (if (null? terms)
           lit
           (match (simplify* (if (= 0 lit) terms (cons lit terms)) simplify-+)
             [(list y) y]
             [(list a ... (? real? b) c ...) (apply expression @+ b (sort (append a c) term<?))]
             [ys (apply expression @+ (sort ys term<?))])))]))
                   
(define $*
  (case-lambda
    [() 1]
    [(x) x]
    [(x y) (or (simplify-* x y) (sort/expression @* x y))]
    [xs 
     (let*-values ([(lits terms) (partition real? xs)]
                   [(lit) (apply * lits)])
       (if (or (zero? lit) (null? terms))
           lit
           (match (simplify* (if (= 1 lit) terms (cons lit terms)) simplify-*)
             [(list y) y]
             [(list a ... (? real? b) c ...) (apply expression @* b (sort (append a c) term<?))]
             [ys (apply expression @* (sort ys term<?))])))]))

(define $- 
  (case-lambda 
    [(x) (match x
           [(? real?) (- x)]
           [(expression (== @-) a) a]
           [(expression (== @*) (? real? c) a) ($* (- c) a)]
           [_ (expression @- x)])]
    [(x y) ($+ x ($- y))]
    [(x . xs) (apply $+ x (map $- xs))]))

(define ($abs x) 
  (match x
    [(? real?) (abs x)]
    [(expression (== @abs) _) x]
    [_ (expression @abs x)]))

(define-lifted-operator @+ $+ T*->T)
(define-lifted-operator @* $* T*->T)
(define-lifted-operator @- $- T*->T)
(define-lifted-operator @abs $abs T*->T)

;; ----------------- Int Operators ----------------- ;; 

(define $quotient (div @quotient $quotient quotient))

(define-syntax-rule (define-remainder $op op @op)
  (define ($op x y)
    (match* (x y)
      [((? integer?) (? integer?)) (op x y)]
      [(_ (≈ 1)) 0]
      [(_ (≈ -1)) 0]
      [((≈ 0) _) 0]
      [(_ (== x)) 0]
      [(_ (expression (== @-) (== x))) 0]
      [((expression (== @-) (== y)) _) 0]
      [((expression (== @*) _ (... ...) (== y) _ (... ...)) _) 0]
      [((expression (== ite) a (? real? b) (? real? c)) (? real?))
       (merge a (op b y) (op c y))]
      [((? real?) (expression (== ite) a 
                              (and b (? real?) (not (? zero?))) 
                              (and c (? real?) (not (? zero?)))))
       (merge a (op x b) (op x c))]
      [(_ _) (expression @op x y)])))
  

(define-remainder $remainder remainder @remainder)
(define-remainder $modulo modulo @modulo)
    
(define T*->integer? (const @integer?))

(define (undefined-for-zero-error name)
  (thunk (raise-arguments-error name "undefined for 0")))
  
(define-syntax-rule (define-lifted-int-operator @op $op op)
  (define-operator @op
    #:identifier 'op
    #:range T*->integer?
    #:unsafe $op
    #:safe (lambda (x y)
             (let ([a (type-cast @integer? x 'op)]
                   [b (type-cast @integer? y 'op)])
               (assert (! ($= b 0)) (undefined-for-zero-error 'op))
               ($op a b)))))

(define-lifted-int-operator @quotient $quotient quotient)
(define-lifted-int-operator @remainder $remainder remainder)
(define-lifted-int-operator @modulo $modulo modulo)

;; ----------------- Real Operators ----------------- ;; 

(define $/ (div @/ $/ /))

(define T*->real? (const @real?))

(define-operator @/
  #:identifier '/
  #:range T*->real?
  #:unsafe $/
  #:safe (case-lambda 
           [(x) (@/ 1 x)]
           [(x y) (let ([a (type-cast @real? x '/)]
                        [b (type-cast @real? y '/)])
                    (assert (! ($= 0 b)) (undefined-for-zero-error '/))
                    ($/ a b))]
           [(x . ys) (let ([z (type-cast @real? x '/)]
                           [zs (for/list ([y ys]) (type-cast @real? y '/))])
                       (for ([z zs])
                         (assert (! ($= z 0)) (undefined-for-zero-error '/)))
                       ($/ x (apply $* zs)))]))
                 
;; ----------------- Coercion Operators ----------------- ;; 

(define (integer->real i)
  (match i
    [(? integer?) i]  
    [(? term?) (expression @integer->real i)]))

(define (real->integer r)
  (match r
    [(? real?) (floor r)]
    [(expression (== @integer->real) x) x]
    [(expression (== ite) a 
                 (expression (== @integer->real) x) 
                 (expression (== @integer->real) y)) (ite a x y)]
    [(expression (== ite) a (expression (== @integer->real) x) y) (ite a x (real->integer y))]
    [(expression (== ite) a x (expression (== @integer->real) y)) (ite a (real->integer x) y)] 
    [(? term?) (expression @real->integer r)]))

(define-operator @integer->real 
  #:identifier 'integer->real
  #:range T*->real?
  #:unsafe integer->real
  #:safe (lambda (n) (integer->real (type-cast @integer? n 'integer->real))))

(define-operator @real->integer 
  #:identifier 'real->integer 
  #:range T*->integer?
  #:unsafe real->integer 
  #:safe (lambda (n) (real->integer (type-cast @real? n 'real->integer))))

;; ----------------- Simplification rules for operators ----------------- ;;

(define (simplify-+ x y)
  (match* (x y)
    [((? real?) (? real?)) (+ x y)]
    [(_ (≈ 0)) x]
    [((≈ 0) _) y]
    [((? expression?) (? expression?)) 
     (or (simplify-+:expr/term x y) (simplify-+:expr/term y x))]
    [((? expression?) _) (simplify-+:expr/term x y)]
    [(_ (? expression?)) (simplify-+:expr/term y x)]
    [(_ _) #f]))

(define (simplify-+:expr/term x y)
  (match* (x y)
    [((expression (== @-) (== y)) _) 0]
    [((expression (== @-) (expression (== @+) (== y) z)) _) ($- z)]
    [((expression (== @-) (expression (== @+) z (== y))) _) ($- z)]
    [((expression (== @+) (expression (== @-) (== y)) z) _) z]
    [((expression (== @+) z (expression (== @-) (== y))) _) z]
    [((expression (== @+) (? real? a) b) (? real?)) ($+ (+ a y) b)]
    [((expression (== ite) a (? real? b) (? real? c)) (? real?)) (ite a (+ b y) (+ c y))]
    [((expression (== @*) (? real? a) (== y)) _) ($* (+ a 1) y)]
    [((expression (== @*) (? real? a) b) (expression (== @*) (? real? c) b)) ($* (+ a c) b)]
    [((expression (== @+) a b) (expression (== @-) a)) b]
    [((expression (== @+) a b) (expression (== @-) b)) a]
    [((expression (== @+) as ...) (expression (== @+) bs ...))
     (let ([alen (length as)] 
           [blen (length bs)])
       (and (<= alen blen) (<= (- blen alen) 1)
            (match (cancel+ as bs)
              [(list) 0]
              [(list b) b]
              [#f #f])))]
    [(_ _) #f]))

(define (cancel+ xs ys) 
  (and ys
       (match xs
         [(list) ys]
         [(list x rest ...)
          (cancel+ rest
                   (match* (x ys)
                     [((? real?) (list (? real? a) b ...)) (and (zero? (+ x a)) b)]
                     [((? term?) (list a ... (expression (== @-) (== x)) b ...)) (append a b)]
                     [((expression (== @-) y) (list a ... y b ...)) (append a b)]
                     [((expression (== @*) (? real? a) b) 
                       (list c ... (expression (== @*) (and (? real?) (app - a)) b) d ...))
                      (append c d)]
                     [(_ _) #f]))])))

(define (simplify-* x y) 
  (match* (x y)
    [((? real?) (? real?)) (* x y)]
    [((≈ 0) _) 0]
    [((≈ 1) _) y]
    [((≈ -1) _) ($- y)]
    [(_ (≈ 0)) 0]
    [(_ (≈ 1)) x]
    [(_ (≈ -1)) ($- x)]
    [((? expression?) (? expression?)) 
     (or (simplify-*:expr/term x y) (simplify-*:expr/term y x))]
    [((? expression?) _) (simplify-*:expr/term x y)]
    [(_ (? expression?)) (simplify-*:expr/term y x)]
    [(_ _) #f]))

(define (simplify-*:expr/term x y)
  (match* (x y)    
    [((expression (== @/) a (== y)) _) a]
    [((expression (== @/) a (expression (== @*) (== y) z)) _) ($/ a z)]
    [((expression (== @/) a (expression (== @*) z (== y))) _) ($/ a z)]
    [((expression (== @/) (? real? a) b) (? real?)) ($/ (* a y) b)]
    [((expression (== @*) (expression (== @/) a (== y)) z) _) ($* a z)]
    [((expression (== @*) z (expression (== @/) a (== y))) _) ($* a z)]
    [((expression (== @*) (? real? a) b) (? real?)) ($* (* a y) b)]
    [((expression (== ite) a (? real? b) (? real? c)) (? real?)) (ite a (* b y) (* c y))]
    [((expression (== @*) a b) (expression (== @/) c a)) ($* b c)]
    [((expression (== @*) a b) (expression (== @/) c b)) ($* a c)]    
    [((expression (== @*) as ...) (expression (== @*) bs ...))
     (let ([alen (length as)] 
           [blen (length bs)])
       (and (<= alen blen) (<= (- blen alen) 1)
            (match (cancel* as bs)
              [(list) 1]
              [(list b) b]
              [#f #f])))]
    [(_ _) #f]))

(define (cancel* xs ys) ;(printf "cancel* ~a ~a\n" xs ys)
  (and ys
       (match xs
         [(list) ys]
         [(list x rest ...)
          (cancel* rest
                   ; Pattern matching broken in 6.1 when the first rule is in the third position.
                   ; TODO: place the first rule in 3rd position and test with 6.2.
                   (match* (x ys) 
                     [((expression (== @/) (≈ 1) c) (list a ... c b ...))
                      (append a b)]
                     [((? term?) (list a ... (expression (== @/) 1 (== x)) b ...)) (append a b)]
                     [((? real?) (list (? real? a) b ...)) (and (= 1 (* x a)) b)]  
                     [(_ _) #f]))])))

(define-syntax-rule (div @op $op op)
  (lambda (x y)
    (match* (x y)
      [((? real?) (? real?)) (op x y)]
      [((≈ 0) _) 0]
      [(_ (≈ 1)) x]
      [(_ (≈ -1)) ($- x)]
      [(_ (== x)) 1]
      [(_ (expression (== @-) (== x))) -1]
      [((expression (== @-) (== y)) _) -1]
      [((expression (== ite) a (? real? b) (? real? c)) (? real?))
       (merge a (op b y) (op c y))]
      [((? real?) (expression (== ite) a 
                              (and b (? real?) (not (? zero?))) 
                              (and c (? real?) (not (? zero?)))))
       (merge a (op x b) (op x c))]
      [((expression (== @op) a (? real? b)) (? real?)) ($op a (* b y))]
      [((expression (== @*) a (... ...) (== y) b (... ...)) _) (apply $* (append a b))]
      [((expression (== @*) as (... ...)) (expression (== @*) bs (... ...))) 
       (or (and (<= (length bs) (length as))
                (let ([cs (cancel-div bs as)])
                  (and cs (apply $* cs))))
           (expression @op x y))]
      [(_ _) (expression @op x y)])))

(define (cancel-div xs ys)  
  (and ys
       (match xs
         [(list) ys]
         [(list x rest ...)
          (cancel-div rest
                      (match* (x ys)
                        [((? real?) (list (== x) b ...)) b]
                        [(_ (list a ... (== x) b ...)) (append a b)]
                        [(_ _) #f]))])))

