#lang racket

(require "term.rkt" "union.rkt" "any.rkt" "bitwise.rkt" "op.rkt")

(provide 
 @boolean? ; (and/c type? (-> any/c @boolean?))
 @false?   ; (-> any/c @boolean?)
 !            ; (and/c op? (-> @boolean? @boolean?))
 && ||        ; (and/c op? (->* ()() #:rest (listof @boolean?) @boolean?))
 => <=>       ; (and/c op? (-> @boolean? @boolean? @boolean?))
 and-&& or-|| 
 instance-of?)

(define (bool/cast v)
  (match v
    [(? boolean?) (values #t v)]
    [(term _ (== @boolean?))  (values #t v)]
    [(union : [g (and (app type-of (== @boolean?)) u)] _ ...) (values g u)]
    [_ (values #f v)]))

(define (bool/compress force? ps)  ; force? is ignored since booleans are immutable and therefore always merged
  (match ps
    [(list _) ps]
    [(list (cons g v) (cons u w)) (list (cons (|| g u) (|| (&& g v) (&& u w))))]
    [_ (list (cons (apply || (map car ps)) 
                   (apply || (for/list ([p ps]) (&& (car p) (cdr p))))))]))

(define (bool/eq? x y) (<=> x y))

(define-primitive-type @boolean? 
  #:pred     (instance-of? boolean? @boolean?) 
  #:least-common-supertype (lambda (t) (if (eq? t @boolean?) @boolean? @any?))
  #:eq?      bool/eq?
  #:equal?   bool/eq?
  #:cast     bool/cast
  #:compress bool/compress) 

(define (true? x) (eq? x #t))
(define-not ! '! @boolean? boolean? not)
(define-and && '&& || ! #t @boolean? boolean? (lambda args (andmap true? args)))
(define-or  || '\|\| && ! #f @boolean? boolean? (lambda args (ormap true? args)))

(define-op => 
  #:type (op/-> (@boolean? @boolean?) @boolean?) 
  #:op   (lambda (x y) (|| (! x) y)))

(define-op <=> 
  #:type (op/-> (@boolean? @boolean?) @boolean?) 
  #:op   (lambda (x y) ;(|| (&& x y) (&& (! x) (! y))))))
           (cond [(equal? x y) #t]
                 [(boolean? x) (if x y (! y))]
                 [(boolean? y) (if y x (! x))] 
                 [(cancel? ! x y) #f]
                 [(term<? x y)  (expression <=> x y)]
                 [else         (expression <=> y x)])))

(define-syntax (and-&& stx)
  (syntax-case stx ()
    [(_) #'#t]
    [(_ v0 #:rest (r ...)) #'(let ([t0 v0]) (and t0 (&& r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) #'(let ([t0 v0]) (and t0 (and-&& v ... #:rest (r ... t0))))]
    [(_ v0) #'v0]
    [(_ v0 v ...) #'(let ([t0 v0]) (and t0 (and-&& v ... #:rest (t0))))]))

(define-syntax (or-|| stx)
  (syntax-case stx ()
    [(_) #'#f]
    [(_ v0 #:rest (r ...)) #'(let ([t0 v0]) (or (equal? #t t0) (|| r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) #'(let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (r ... t0))))]
    [(_ v0) #'v0]
    [(_ v0 v ...) #'(let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (t0))))]))

(define (@false? v) 
  (or (false? v)  
      (and (typed? v)
           (let-values ([(g b) (bool/cast v)])
             (and g (&& g (! b)))))))

(define-syntax-rule (instance-of? primitive-type ... symbolic-type)
  (match-lambda [(? primitive-type) #t] ...
                [(and (? typed? v) (app get-type t)) 
                 (or (and t (subtype? t symbolic-type)) 
                     (and (union? v) (apply || (for/list ([g (in-union-guards v symbolic-type)]) g))))]
                [_ #f]))
