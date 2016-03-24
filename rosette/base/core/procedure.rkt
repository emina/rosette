#lang racket

(require 
  racket/provide 
  (for-syntax racket/syntax (only-in "lift.rkt" with@)) 
  (only-in "type.rkt" define-lifted-type typed? get-type subtype? type-applicable? @any/c)
  (only-in "bool.rkt" || @false?)
  (only-in "union.rkt" union union? in-union-guards union-filter union-guards)
  (only-in "safe.rkt" assert argument-error)
  (only-in "forall.rkt" guard-apply))

(provide (filtered-out with@ (all-defined-out)))

(define-lifted-type @procedure?
  #:base procedure?
  #:is-a? (match-lambda [(and (? typed?) (app get-type t) v) 
                         (or (subtype? t @procedure?)
                             (and (union? v) 
                                  (subtype? @procedure? t)
                                  (apply || (for/list ([g (in-union-guards v @procedure?)]) g))))]
                        [(? procedure?) #t] 
                        [_ #f])
  #:methods
  [(define (least-common-supertype self other) 
    (if (or (equal? other @procedure?) (type-applicable? other)) 
        @procedure? 
        @any/c))
   (define (type-applicable? self) #t)
   (define (type-eq? self v0 v1) (eq? v0 v1))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(union _ (== @procedure?)) v]
       [(union _ (? (curryr subtype? @procedure?))) v]
       [(union vs (? (curry subtype? @procedure?)))
        (match (union-filter v @procedure?)
          [(union (list (cons g u)))
           (assert g (argument-error caller "procedure?" v))
           u]
          [r
           (assert (apply || (union-guards r)) (argument-error caller "procedure?" v))
           r])]
       [(? procedure?) v]
       [_ (assert #f (argument-error caller "procedure?" v))]))
   (define (type-compress self force? ps)
     (if force? (procedure/unsafe-compress ps) ps))])

(define (accepts-keywords? guarded-proc)
  (let-values ([(required accepted) (procedure-keywords (cdr guarded-proc))])
    (not (null? accepted))))

(define (common-arity guarded-procs)
  (let ([arity (procedure-arity (cdr (car guarded-procs)))])
    (and (integer? arity) 
         (for/and ([p (cdr guarded-procs)]) 
           (equal? arity (procedure-arity (cdr p)))) 
         arity)))

(define (procedure/unsafe-compress ps)
  (define good (apply || (map car ps)))
  (cond [(ormap accepts-keywords? ps)
         (make-keyword-procedure 
          (lambda (kws kw-args . rest)
            (assert good)
            (guard-apply (lambda (p) (keyword-apply p kws kw-args rest)) ps)))]
        [else
         (case (common-arity ps)
           [(0)  (lambda () (assert good) (guard-apply (lambda (p) (p)) ps))]
           [(1)  (lambda (x) (assert good) (guard-apply (lambda (p) (p x)) ps))]
           [(2)  (lambda (x y) (assert good) (guard-apply (lambda (p) (p x y)) ps))]
           [(3)  (lambda (x y z) (assert good) (guard-apply (lambda (p) (p x y z)) ps))]
           [else (lambda xs (assert good) (guard-apply (lambda (p) (apply p xs)) ps))])]))

(define (@procedure-rename proc name)
  (match proc
    [(union gvs)    (guard-apply (curryr procedure-rename name) gvs)]
    [(? procedure?) (procedure-rename proc name)]))

(define (@negate f)
  (unless (@procedure? f) (raise-argument-error 'negate "procedure?" f))
  (let-values ([(arity) (procedure-arity f)] [(_ kwds) (procedure-keywords f)])
    (case (and (null? kwds) arity) ; optimize some simple cases
      [(0) (lambda () (@false? (f)))]
      [(1) (lambda (x) (@false? (f x)))]
      [(2) (lambda (x y) (@false? (f x y)))]
      [else (compose1 @false? f)])))

(define (@void? v)
  (match v
    [(? void?) #t]
    [(union vs) (apply || (for/list ([gv vs] #:when (void? (cdr gv))) (car gv)))]
    [_ #f]))
  

