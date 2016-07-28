#lang racket

(require "term.rkt" "union.rkt")

(provide @boolean? @false? 
         ! && || => <=> @! @&& @|| @=> @<=> @exists @forall
         and-&& or-|| instance-of?
         @assert pc with-asserts with-asserts-only 
         (rename-out [export-asserts asserts]) clear-asserts!
         T*->boolean?)

;; ----------------- Boolean type ----------------- ;; 
(define-lifted-type @boolean? 
  #:base boolean?
  #:is-a? (instance-of? boolean? @boolean?)
  #:methods
  [(define (solvable-default self) #f)
   (define (type-eq? self u v) (<=> u v)) 
   (define (type-equal? self u v) (<=> u v))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? boolean?) v]
       [(term _ (== self)) v]
       [(union : [g (and (or (? boolean?) (term _ (== self))) u)] _ ...)
        (@assert g (thunk (raise-argument-error caller "expected a boolean?" v)))
        u]
       [_  (@assert #f (thunk (raise-argument-error caller "expected a boolean?" v)))]))
   (define (type-compress self force? ps)
     (match ps
       [(list _) ps]
       [(list (cons g v) (cons u w)) (list (cons (|| g u) (|| (&& g v) (&& u w))))]
       [_ (list (cons (apply || (map car ps)) 
                      (apply || (for/list ([p ps]) (&& (car p) (cdr p))))))]))])

;; ----------------- Lifting utilities ----------------- ;; 

(define (lift-op op)
  (define caller (object-name op))
  (case (procedure-arity op)
    [(1)  (lambda (x) (op (type-cast @boolean? x caller)))]
    [(2)  (lambda (x y) (op (type-cast @boolean? x caller) (type-cast @boolean? y caller)))]
    [else (case-lambda [() (op)]
                       [(x) (op (type-cast @boolean? x caller))]
                       [(x y) (op (type-cast @boolean? x caller) (type-cast @boolean? y caller))]
                       [xs (apply op (for/list ([x xs]) (type-cast @boolean? x caller)))])]))

; A generic typing procedure for a lifted operator that takes N >= 0 arguments of type T
; and returns a @boolean?. See term.rkt.
(define (T*->boolean? . xs) @boolean?)

(define-syntax-rule (define-lifted-operator @op $op)
  (define-operator @op
    #:identifier '$op
    #:range T*->boolean?
    #:unsafe $op
    #:safe (lift-op $op)))

(define-syntax-rule (define-quantifier @op $op)
  (define-operator @op
    #:identifier '$op
    #:range T*->boolean?
    #:unsafe $op
    #:safe
    (lambda (@vars @body)
      (match* (@vars (type-cast @boolean? @body '$op))
        [((list (constant _ (? primitive-solvable?)) (... ...)) body)
         ($op @vars body)]
        [(_ _)
         (@assert
          #f
          (thunk
           (raise-argument-error
            '$op
            "expected a list of symbolic constants of primitive solvable types" @vars)))]))))

;; ----------------- Basic boolean operators ----------------- ;; 
(define (! x)
  (match x
    [(? boolean?) (not x)]
    [(expression (== @!) y) y]
    [_ (expression @! x)]))

(define && (logical-connective @&& @|| #t #f))
(define || (logical-connective @|| @&& #f #t))

(define (=> x y) (|| (! x) y))

(define (<=> x y) ;(|| (&& x y) (&& (! x) (! y))))))
  (cond [(equal? x y) #t]
        [(boolean? x) (if x y (! y))]
        [(boolean? y) (if y x (! x))]
        [(cancel? x y) #f]
        [(term<? x y) (expression @<=> x y)]
        [else         (expression @<=> y x)]))

(define-lifted-operator @! !)
(define-lifted-operator @&& &&)
(define-lifted-operator @|| ||)
(define-lifted-operator @=> =>)
(define-lifted-operator @<=> <=>)

(define (@false? v)
  (match v
    [#f #t]
    [(term _ (== @boolean?)) (! v)]
    [(union xs (== @any/c))
     (let loop ([xs xs])
       (match xs
         [(list) #f]
         [(list (cons g (and (or (? boolean?) (term _ (== @boolean?))) u)) _ ...)
          (&& g (! u))]
         [_ (loop (cdr xs))]))]
    [_ #f]))

(define exists (quantifier @exists))
(define forall (quantifier @forall))
(define-quantifier @exists exists)
(define-quantifier @forall forall)


       
;; ----------------- Additional operators ----------------- ;; 
(define-syntax and-&&
  (syntax-rules ()
    [(_) #t]
    [(_ v0) v0]
    [(_ v0 #:rest (r ...)) (let ([t0 v0]) (and t0 (@&& r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) (let ([t0 v0]) (and t0 (and-&& v ... #:rest (r ... t0))))]
    [(_ v0 v ...) (let ([t0 v0]) (and t0 (and-&& v ... #:rest (t0))))]))

(define-syntax or-||
  (syntax-rules ()
    [(_) #f]
    [(_ v0) v0]
    [(_ v0 #:rest (r ...)) (let ([t0 v0]) (or (equal? #t t0) (@|| r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) (let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (r ... t0))))]
    [(_ v0 v ...) (let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (t0))))]))

(define-syntax-rule (instance-of? primitive-type ... symbolic-type)
  (match-lambda [(? primitive-type) #t] ...
                [(and (? typed? v) (app get-type t)) 
                 (or (and t (subtype? t symbolic-type)) 
                     (and (union? v) (apply || (for/list ([g (in-union-guards v symbolic-type)]) g))))]
                [_ #f]))


;; ----------------- Partial evaluation rules for ∀ and ∃ ----------------- ;;

(define-syntax-rule (quantifier @op)
  (lambda (vars body)
    (match* (vars body)
      [((list) _) body]
      [(_ (? boolean?)) body]
      [(_ _) (expression @op vars body)])))

;; ----------------- Partial evaluation rules for && and || ----------------- ;; 
(define-syntax-rule (logical-connective op co iden !iden)
  (case-lambda 
    [() iden]
    [(x) x]
    [(x y) 
     (match* (x y)
       [((== iden) _) y]
       [(_ (== iden)) x]
       [((== !iden) _) !iden]
       [(_ (== !iden)) !iden]
       [(_ _)
        (first-value 
         (simplify-connective op co !iden x y)
         (if (term<? x y)  (expression op x y) (expression op y x)))])]
    [xs 
     (cond [(member !iden xs) !iden]
           [else 
            (match (simplify-fp op co !iden (remove-duplicates (filter term? xs)))
              [(list) iden]
              [(list x) x]
              [ys (apply expression op (sort ys term<?))])])]))
       
(define ⊥ (void))
(define-syntax first-value
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (let ([v e0])
                    (if (void? v)
                        (first-value e ...)
                        v))]))

(define (simplify-connective op co !iden x y)
  (match* (x y)
    [(_ (== x)) x]
    [((? expression?) (? expression?))
     (first-value
      (simplify-connective:expr/any op co !iden x y)
      (simplify-connective:expr/any op co !iden y x)
      (simplify-connective:expr/expr op co !iden x y))]
    [((? expression?) _)
     (simplify-connective:expr/any op co !iden x y)]
    [(_ (? expression?))
     (simplify-connective:expr/any op co !iden y x)]
    [(_ _) ⊥]))
      
(define (simplify-connective:expr/any op co !iden x y)
  (match x 
    [(expression (== @!) (== y)) !iden]
    [(expression (== co) _ ... (== y) _ ...) y]
    [(expression (== op) _ ... (== y) _ ...) x]
    [(expression (== op) _ ... (expression (== @!) (== y)) _ ...) !iden]
    [(expression (== @!) (expression (== co) _ ... (== y) _ ...)) !iden]
    [(expression (== @!) (expression (== co) _ ... (expression (== @!) (== y)) _ ...)) x]
    [(expression (== @!) (expression (== op) _ ... (expression (== @!) (== y)) _ ...)) y]
    [(expression (== @!) a) 
     (match y 
       [(expression (== op) _ ... (== a) _ ...) !iden]
       [_ ⊥])]
    [_ ⊥]))

; Applies the following simplification rules symmetrically:
; (1) (op (op a1 ... an) (op ai ... aj)) ==> (op a1 ... an)
; (2) (op (op a1 ... ai ... an) (op b1 ... (neg ai) ... bn) ==> !iden
; (3) (op (co a1 ... an) (co ai ... aj)) ==> (co ai ... aj)
; Returns ⊥ if none of the rules applicable; otherwise returns the simplified result.
(define (simplify-connective:expr/expr op co !iden a b)
  (match* (a b)
    [((expression (== op) xs ...) (expression (== op) ys ...))
     (cond [(sublist? xs ys) b]
           [(sublist? ys xs) a]
           [(for*/or ([x xs][y ys]) (cancel? x y)) !iden]
           [else ⊥])]
    [((expression (== co) xs ...) (expression (== co) ys ...))
     (cond [(sublist? xs ys) a]
           [(sublist? ys xs) b]
           [else ⊥])]
    [(_ _) ⊥]))
    
(define (simplify-fp op co !iden xs)
  (or
   (and (> (length xs) 10) xs)
   (let-values ([(!ys ys) (for/fold ([!ys '()][ys '()]) ([x xs])
                            (match x
                              [(expression (== @!) y) (values (cons y !ys) ys)]
                              [_ (values !ys (cons x ys))]))])
     (for/first ([!y !ys] #:when (member !y ys)) (list !iden)))
   (let outer ([xs xs])
     (match xs
       [(list x rest ..1)
        (let inner ([head rest] [tail '()])
          (match head
            [(list) (match (outer tail)
                      [(and (list (== !iden)) t) t]
                      [t (cons x t)])]
            [(list y ys ...)
             (match (simplify-connective op co !iden x y)
               [(== ⊥) (inner ys (cons y tail))]
               [(== !iden) (list !iden)]
               [v (outer (cons v (append ys tail)))])]))]
       [_ xs]))))
            
(define (cancel? a b)
  (match* (a b)
    [(_ (expression (== @!) (== a))) #t]
    [((expression (== @!) (== b)) _) #t]
    [(_ _) #f]))

  
;; ----------------- Assertions and path condition ----------------- ;; 
(define (export-asserts) (remove-duplicates (asserts)))

(define (clear-asserts!)  (asserts '()))
    
(define asserts 
  (make-parameter 
   '()
   (match-lambda [(? list? xs) xs]
                 [x (if (eq? x #t) (asserts) (cons x (asserts)))])))

(define pc 
  (make-parameter 
   #t
   (lambda (new-pc) 
     (or (boolean? new-pc) 
         (and (term? new-pc) (equal? @boolean? (term-type new-pc)))
         (error 'pc "expected a boolean path condition, given a ~s" (type-of new-pc)))
     (or (&& (pc) new-pc)
         (error 'pc "infeasible path condition")))))

(define-syntax (@assert stx)
  (syntax-case stx ()
    [(_ val) (syntax/loc stx (@assert val #f))]
    [(_ val msg) 
     (syntax/loc stx 
       (let ([guard (not-false? val)])
         (asserts (=> (pc) guard)) 
         (when (false? guard)
           (raise-assertion-error msg))))]))

(define (not-false? v)
  (or (eq? v #t) (! (@false? v))))

(define (raise-assertion-error msg)
  (if (procedure? msg)
      (msg)
      (error 'assert (if msg (format "~a" msg) "failed"))))
                     
(define-syntax (with-asserts stx)
  (syntax-case stx (begin)
    [(_ (begin form ...)) #'(with-asserts (let () form ...))]
    [(_ form) #`(parameterize ([asserts (asserts)])
                  (let* ([val form]
                         [bools (remove-duplicates (asserts))])
                    (values val bools)))]))

(define-syntax-rule (with-asserts-only form)
  (let-values ([(out asserts) (with-asserts form)])
    asserts))
