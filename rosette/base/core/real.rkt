#lang racket

(require (for-syntax racket/syntax) racket/stxparam racket/stxparam-exptime)
(require "term.rkt" "op.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" 
         "merge.rkt" "safe.rkt" "lift.rkt" "forall.rkt")

;; ----------------- Integer and Real Types ----------------- ;; 

(define-lifted-type @real?
  #:base real?
  #:is-a? (instance-of? real? @real?)
  #:methods
  [(define (least-common-supertype self t)
     (if (or (equal? self t) (equal? @integer? t)) self @any/c))
   (define (type-eq? self u v) (@= u v)) 
   (define (type-equal? self u v) (@= u v))
   (define (cast self v)
     (match v
       [(? real?) (values #t v)]
       [(term _ (== self)) (values #t v)]
       [(term _ (== @integer?)) (values #t (int->real v))]
       [(union xs (or (== @real?) (== @any/c)))
        (let ([rs (for/list ([gx (in-union v self)])
                    (match gx
                      [(cons g (and (term _ (== @integer?)) x)) (cons g (int->real x))]
                      [_ gx]))])
          (match rs
            [(list) (values #f v)]
            [(list (cons g r)) (values g r)]
            [(app length (== (length xs))) (values #t (apply merge* rs))]
            [(list (cons g _) ...) (values (apply || g) (apply merge* rs))]))]
       [_ (values #f v)]))
   (define (type-compress self force? ps) (generic-merge @+ 0 ps))])
  
(define-lifted-type @integer?
  #:base integer?
  #:is-a? int?
  #:methods 
  [(define (least-common-supertype self t)
     (if (or (equal? self t) (equal? @real? t)) t @any/c))
   (define (type-eq? self u v) (@= u v)) 
   (define (type-equal? self u v) (@= u v))
   (define (cast self v)
     (match v
       [(? integer?) (values #t v)]
       [(term _ (== self)) (values #t v)]
       [(union : [g (and (or (? integer?) (term _ (== self))) u)] _ ...) (values g u)]
       [_ (values #f v)]))
   (define (type-compress self force? ps) (generic-merge @+ 0 ps))])

(define (@= a b) #f)
(define (@+ a b) #f)
(define (int->real n) #f)
(define (real->int n) #f)

(define (int? v)
  (match v
    [(? integer?) #t]
    [(term _ (== @integer?)) #t]
    [(term _ (== @real?)) (expression @int? v)]
    [(union xs (or (== @real?) (== @any/c)))
     (apply || (for/list ([gx xs]) (&& (car gx) (int? (cdr gx)))))]
    [_ #f]))
   
(define-operator @int?
    #:name int?
    #:type T*->boolean?
    #:unsafe int?
    #:safe int?)
  