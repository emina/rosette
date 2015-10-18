#lang racket

(require 
  racket/provide 
  (for-syntax racket/syntax (only-in "../core/lift.rkt" with@)) 
  (only-in "../core/generic.rkt" make-cast)
  (only-in "../core/type.rkt" lift-type typed? get-type subtype? type-applicable? @any/c)
  (only-in "../core/bool.rkt" ||)
  (only-in "../core/union.rkt" union union? in-union-guards union-filter union-guards)
  (only-in "../core/safe.rkt" assert)
  (only-in "../core/forall.rkt" guard-apply)
  (only-in "../form/control.rkt" @not))

(provide (filtered-out with@ (all-defined-out)))

(define (is-procedure? v)
  (match v
    [(and (? typed?) (app get-type t)) 
     (and t 
          (or (subtype? t @procedure?)
              (and (union? v) 
                   (subtype? @procedure? t)
                   (apply || (for/list ([g (in-union-guards v @procedure?)]) g)))))]
    [(? procedure?) #t] 
    [_ #f]))

(define (procedure/cast v)
  (match v
    [(union _ (== @procedure?)) (values #t v)]
    [(union _ (? (curryr subtype? @procedure?))) (values #t v)]
    [(union vs (? (curry subtype? @procedure?)))
     (match (union-filter v @procedure?)
       [(union (list (cons g u))) (values g u)]
       [r (values (apply || (union-guards r)) r)])]
    [(? procedure?) (values #t v)]
    [_ (values #f v)]))
  
(define (procedure/compress force? ps)
  (if force? (procedure/unsafe-compress ps) ps))

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
               
(define @procedure?
  (lift-type
   procedure?
   #:is-a? is-procedure?
   #:least-common-supertype 
   (lambda (t) 
     (if (or (eq? t @procedure?) (type-applicable? t)) 
         @procedure? 
         @any/c))
   #:eq? eq?
   #:equal? equal?
   #:applicable? #t
   #:cast procedure/cast
   #:compress procedure/compress))
  
(define (@procedure-rename proc name)
  (match proc
    [(union gvs)    (guard-apply (curryr procedure-rename name) gvs)]
    [(? procedure?) (procedure-rename proc name)]))

(define (@negate f)
  (unless (@procedure? f) (raise-argument-error 'negate "procedure?" f))
  (let-values ([(arity) (procedure-arity f)] [(_ kwds) (procedure-keywords f)])
    (case (and (null? kwds) arity) ; optimize some simple cases
      [(0) (lambda () (@not (f)))]
      [(1) (lambda (x) (@not (f x)))]
      [(2) (lambda (x y) (@not (f x y)))]
      [else (compose1 @not f)])))

(define (@void? v)
  (match v
    [(? void?) #t]
    [(union vs) (apply || (for/list ([gv vs] #:when (void? (cdr gv))) (car gv)))]
    [_ #f]))
  

