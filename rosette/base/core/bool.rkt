#lang racket

(require "term.rkt" "union.rkt" "op.rkt")

(provide @boolean? @false? ! && || => <=>  and-&& or-|| instance-of?)

(define-lifted-type @boolean? 
  #:base boolean?
  #:is-a? (instance-of? boolean? @boolean?)
  #:methods
  [(define (type-eq? self u v) (<=> u v)) 
   (define (type-equal? self u v) (<=> u v))
   (define (cast self v) 
     (match v
       [(? boolean?) (values #t v)]
       [(term _ (== self))  (values #t v)]
       [(union : [g (and (or (? boolean?) (term _ (== self))) u)] _ ...) (values g u)]
       [_ (values #f v)]))
   (define (type-compress self force? ps)
     (match ps
       [(list _) ps]
       [(list (cons g v) (cons u w)) (list (cons (|| g u) (|| (&& g v) (&& u w))))]
       [_ (list (cons (apply || (map car ps)) 
                      (apply || (for/list ([p ps]) (&& (car p) (cdr p))))))]))])

(define binary-type (op/-> (@boolean? @boolean?) @boolean?)) 
(define nary-type (op/-> (#:rest @boolean?) @boolean?)) 

(define-op !
  #:type (op/-> (@boolean?) @boolean?)
  #:op   (lambda (x)
           (match x
             [(? boolean?) (not x)]
             [(expression (== !) y) y]
             [_ (expression ! x)])))

(define-op &&
  #:type nary-type
  #:op   (logical-connective && || #t #f)) 

(define-op ||
  #:name '\|\|
  #:type nary-type
  #:op   (logical-connective || && #f #t)) 
             
(define-op => 
  #:type binary-type
  #:op   (lambda (x y) (|| (! x) y)))

(define-op <=> 
  #:type binary-type
  #:op   (lambda (x y) ;(|| (&& x y) (&& (! x) (! y))))))
           (cond [(equal? x y) #t]
                 [(boolean? x) (if x y (! y))]
                 [(boolean? y) (if y x (! x))]
                 [(cancel? x y) #f]
                 [(term<? x y) (expression <=> x y)]
                 [else         (expression <=> y x)])))

(define (@false? v) 
  (or (false? v)  
      (and (typed? v)
           (let-values ([(g b) (cast @boolean? v)])
             (and g (&& g (! b)))))))

(define-syntax and-&&
  (syntax-rules ()
    [(_) #t]
    [(_ v0) v0]
    [(_ v0 #:rest (r ...)) (let ([t0 v0]) (and t0 (&& r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) (let ([t0 v0]) (and t0 (and-&& v ... #:rest (r ... t0))))]
    [(_ v0 v ...) (let ([t0 v0]) (and t0 (and-&& v ... #:rest (t0))))]))

(define-syntax or-||
  (syntax-rules ()
    [(_) #f]
    [(_ v0) v0]
    [(_ v0 #:rest (r ...)) (let ([t0 v0]) (or (equal? #t t0) (|| r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) (let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (r ... t0))))]
    [(_ v0 v ...) (let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (t0))))]))

(define-syntax-rule (instance-of? primitive-type ... symbolic-type)
  (match-lambda [(? primitive-type) #t] ...
                [(and (? typed? v) (app get-type t)) 
                 (or (and t (subtype? t symbolic-type)) 
                     (and (union? v) (apply || (for/list ([g (in-union-guards v symbolic-type)]) g))))]
                [_ #f]))

;; Partial evaluation rules for && and ||.
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
    [(expression (== !) (== y)) !iden]
    [(expression (== co) _ ... (== y) _ ...) y]
    [(expression (== op) _ ... (== y) _ ...) x]
    [(expression (== op) _ ... (expression (== !) (== y)) _ ...) !iden]
    [(expression (== !) (expression (== co) _ ... (== y) _ ...)) !iden]
    [(expression (== !) (expression (== co) _ ... (expression (== !) (== y)) _ ...)) x]
    [(expression (== !) (expression (== op) _ ... (expression (== !) (== y)) _ ...)) y]
    [(expression (== !) a) 
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

; Returns #t if ys contains all elements of xs, in the order 
; in which they occur in xs. Otherwise returns #f.
(define (sublist? xs ys)
  (and (<= (length xs) (length ys))
       (match xs
         [(list) #t]
         [(list x xs ...)
          (match ys 
            [(list _ ... (== x) ys ...) (sublist? xs ys)]
            [_ #f])])))
    
(define (simplify-fp op co !iden xs)
  (or 
   (let-values ([(!ys ys) (partition negated? xs)])
     (and (for/or ([!y !ys]) (member (car (term-child !y)) ys)) 
          (list !iden)))
   (and (> (length xs) 100) xs)
   (let fp ([xs xs])
     (let ([xsimp (simplify-pairwise op co !iden xs)])
       (if (equal? xs xsimp)
           xs
           (fp xsimp))))))

(define (negated? expr)
  (match expr
    [(expression (== !) _ ...) #t]
    [_ #f]))

(define (simplify-pairwise op co !iden args)
  (match args
    [(list x rest ..1)
     (let loop ([xs rest] [simp '()] [simp? #f])
       (match xs 
         [(list) 
          (if simp?
              (simplify-pairwise op co !iden (reverse simp))
              (cons x (simplify-pairwise op co !iden rest)))]
         [(list y ys ...)
          (match (simplify-connective op co !iden x y)
            [(== ⊥) (loop ys (cons y simp) simp?)]
            [(== !iden) (list !iden)]
            [v (loop ys (cons v simp) #t)])]))]
    [_ args]))
            
(define (cancel? a b)
  (match* (a b)
    [(_ (expression (== !) (== a))) #t]
    [((expression (== !) (== b)) _) #t]
    [(_ _) #f]))

  


