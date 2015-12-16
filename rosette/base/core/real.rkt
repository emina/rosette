#lang racket

(require (for-syntax racket/syntax) racket/stxparam racket/stxparam-exptime)
(require "term.rkt" "op.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" 
         "merge.rkt" "safe.rkt" "lift.rkt" "forall.rkt")

(provide @integer? @real? @= @< @<= @>= @> @+ @* @- @div @mod @abs @/
         @integer->real @real->integer)

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
   (define (type-eq? self u v) ($= u v)) 
   (define (type-equal? self u v) ($= u v))
   (define (cast self v)
     (match v
       [(? real?) (values #t v)]
       [(term _ (== self)) (values #t v)]
       [(term _ (== @integer?)) (values #t (integer->real v))]
       [(union xs (or (== @real?) (== @any/c)))
        (let-values ([(i r) (guarded-numbers xs)])
          (match* (i r)
            [((cons g x) #f) (values g (integer->real x))]
            [(#f (cons g x)) (values g x)]
            [((cons gi xi) (cons gr _)) 
             (values (or (= (length xs) 2) (|| gi gr)) (merge* (cons gi (integer->real xi)) r))]
            [(_ _) (values #f v)]))]
       [_ (values #f v)]))
   (define (type-compress self force? ps) (generic-merge @+ 0 ps))])
  
(define-lifted-type @integer?
  #:base integer?
  #:is-a? int?
  #:methods 
  [(define (least-common-supertype self t)
     (if (or (equal? self t) (equal? @real? t)) t @any/c))
   (define (type-eq? self u v) ($= u v)) 
   (define (type-equal? self u v) ($= u v))
   (define (cast self v)
     (match v
       [(? integer?) (values #t v)]
       [(term _ (== self)) (values #t v)]
       [(term _ (== @real?)) 
        (let ([g (int? v)])
          (if g (values g (real->integer v)) (values #f v)))]
       [(union xs (or (== @real?) (== @any/c)))
        (let-values ([(i r) (guarded-numbers xs)])
          (match* (i r)
            [((cons g x) #f) (values g x)]
            [(#f (cons g x)) 
             (let ([g (&& g (int? x))])
               (if g (values g (real->integer x)) (values #f v)))]
            [((cons gi xi) (cons gr xr))
             (let ([gr (&& (int? xr) gr)])
               (if gr 
                   (values (or (= (length xs) 2) (|| gi gr)) (merge* i (cons gr (real->integer xr))))
                   (values gi xi)))]
            [(_ _) (values #f v)]))]
       [_ (values #f v)]))
   (define (type-compress self force? ps) (generic-merge @+ 0 ps))])

;; ----------------- Lifting Utilities ----------------- ;; 
(define (guarded-numbers xs)
  (for/fold ([i #f][r #f]) ([gx xs])
    (match (cdr gx)
      [(or (? integer?) (term _ (== @integer?))) (values gx r)]
      [(or (? real?) (term _ (== @real?))) (values i gx)]
      [_ (values i r)])))

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
         [(_ _) (assert #f (numeric-type-error caller @real? v))]))]))

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
  (match a 
    [(? int-primitive?)
     (match b
       [(? int-primitive?)  (op a b)]
       [(? real-primitive?) (op (integer->real a) b)]
       [(union (list-no-order (cons gv (? int-primitive? v)) (cons gw w)))
        (merge* (cons gv (op a v)) (cons gw (op (integer->real a) w)))])]
    [(? real-primitive?)   (op a (coerce b @real? caller))]
    [(union (list-no-order (cons gv (? int-primitive? v)) (cons gw w)))
     (match b
       [(? int-primitive?)  (merge* (cons gv (op v b)) (cons gw (op w (integer->real b))))]
       [(? real-primitive?) (op (coerce a @real? caller) b)]
       [(union (list-no-order (cons gc (? int-primitive? c)) (cons gd d)))
        (let* ([gi (&& gv gc)]
               [!gi (! gi)])
          (cond [(and (term? gi) (term? !gi))
                 (merge* (cons gi (op v c)) 
                         (cons !gi (op (coerce a @real? caller) (coerce b @real? caller))))]
                [(false? !gi)
                 (assert gi (numeric-type-error caller @real? x y))
                 (op v c)]
                [else ; (false? gi)
                 (assert !gi (numeric-type-error caller @real? x y))
                 (op (coerce a @real? caller) (coerce b @real? caller))]))])]))
                            

(define (safe-apply-n op xs)
  (define caller (object-name op))
  (define ys (for/list ([x xs]) (numeric-coerce x caller)))
  (match ys
    [(or (list (? int-primitive?) ...) (list (? real-primitive?) ...)) (apply op ys)]
    [(list _ ... (and (not (? int-primitive?)) (? real-primitive?)) _ ...) 
     (apply op (for/list ([y ys]) (coerce y @real? caller)))] 
    [_ 
     (define-values (g* i*)
       (for/lists (g* i*) ([y ys])
         (match y
           [(? int-primitive?) (values #t y)]
           [(union (list-no-order (cons g (? int-primitive? v))) _) (values g v)])))
     (define g (apply && g*))
     (define !g (! g))
     (cond [(and (term? g) (term? !g))
            (merge* (cons g (apply op i*)) 
                    (cons !g (apply op (for/list ([y ys]) (coerce y @real? caller)))))]
           [(false? !g)
            (assert g (numeric-type-error caller @real? xs))
            (apply op i*)]
           [else ; (false? g)
            (assert !g (numeric-type-error caller @real? xs))
            (apply op (for/list ([y ys]) (coerce y @real? caller)))])]))
     
(define (lift-op op)
  (case (procedure-arity op)
    [(1)  (lambda (x) (safe-apply-1 op x))]
    [(2)  (lambda (x y) (safe-apply-2 op x y))]
    [else (case-lambda [() (op)]
                       [(x) (safe-apply-1 op x)]
                       [(x y) (safe-apply-2 op x y)]
                       [xs (safe-apply-n op xs)])]))

(define-syntax-rule (define-lifted-operator @bvop bvop type)
  (define-operator @bvop
    #:name (string->symbol (substring (symbol->string '@bvop) 1))
    #:type type
    #:unsafe bvop
    #:safe (lift-op bvop)))
    
;; ----------------- Predicates ----------------- ;; 

(define-operator @int?
    #:name 'int?
    #:type T*->boolean?
    #:unsafe int?
    #:safe int?)

(define $=  (compare @= = sort/expression))
(define $<= (compare @<= <= expression))
(define $<  (compare @< < expression))
(define ($>= x y) ($<= y x))
(define ($> x y) ($< y x)) 

(define-syntax-rule (compare @op op expr)
  (lambda (x y)
    (match* (x y)
      [((? real?) (? real?)) (op x y)]
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
      [(_ _) (expr @op x y)])))

(define-lifted-operator @=  $= T*->boolean?)
(define-lifted-operator @<= $<= T*->boolean?)
(define-lifted-operator @>= $>= T*->boolean?)
(define-lifted-operator @<  $< T*->boolean?)
(define-lifted-operator @>  $> T*->boolean?)

;; ----------------- Int and Real Operators ----------------- ;; 

(define ($+ a b) (+ a b))
(define ($* a b) (* a b))
(define ($- a b) (- a b))

(define-lifted-operator @+ $+ T*->T)
(define-lifted-operator @* $* T*->T)
(define-lifted-operator @- $- T*->T)

;; ----------------- Int Operators ----------------- ;; 

(define ($div a b) (quotient a b))
(define ($mod a b) (remainder a b))
(define ($abs a) (abs a))

(define-lifted-operator @div void T*->T)
(define-lifted-operator @mod void T*->T)
(define-lifted-operator @abs void T*->T)

;; ----------------- Real Operators ----------------- ;; 

(define ($/ a b) (/ a b))
(define-lifted-operator @/ $/ T*->T)

;; ----------------- Coercion Operators ----------------- ;; 

(define (integer->real i)
  (match i
    [(? integer?) i]
    [(? term?) (expression @integer->real i)]))

(define (real->integer r)
  (match r
    [(? real?) (floor r)]
    [(expression (== @integer->real) x) x]
    [(? term?) (expression @real->integer r)]))

(define-operator @integer->real 
  #:name 'integer->real
  #:type (lambda (i) @real?)
  #:unsafe integer->real
  #:safe (lambda (n) (integer->real (coerce n @integer? 'integer->real))))

(define-operator @real->integer 
  #:name 'real->integer 
  #:type (lambda (r) @integer?)
  #:unsafe real->integer 
  #:safe (lambda (n) (real->integer (coerce n @real? 'real->integer))))


(require "../form/define.rkt")
(define-symbolic a b @boolean?)
(define-symbolic i j @integer?)
(define-symbolic p q @real?)