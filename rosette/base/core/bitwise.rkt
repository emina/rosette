#lang racket

(require (for-syntax racket/syntax) racket/stxparam racket/stxparam-exptime)
(require "term.rkt" "op.rkt")

(provide define-not define-and define-or cancel?
         simplify-connective simplify-connective* sublist?)


; Simplification rules for bitwise and/or. Assumes that 
; neither x nor y are iden or !iden.
(define (simplify-connective op co no literal? !iden x y) 
  (cond [(equal? x y) x]
        [(expression? x)
         (cond [(expression? y)
                (or (simplify-connective:expr/term op co no !iden x y)
                    (simplify-connective:expr/term op co no !iden y x)
                    (match* (x y)
                      [((expression (== op) xs ...) (expression (== op) ys ...))
                       (for*/or ([a xs][b ys])
                         (match* (a b)
                           [(_ (expression (== no) (== a))) !iden]
                           [((expression (== no) (== b)) _) !iden]
                           [((? literal?) (? literal?)) (and (equal? a ((op-unsafe no) b)) !iden)]
                           [(_ _) #f]))]
                      [((expression (== co) xs ...) (expression (== co) ys ...))
                       (cond [(sublist? xs ys) x]
                             [(sublist? ys xs) y]
                             [else #f])]                      
                    [(_ _) #f]))]
               [(constant? y) (simplify-connective:expr/term op co no !iden x y)]
               [else (simplify-connective:expr/lit op co no !iden x y)])]
        [(expression? y)
         (cond [(constant? x) (simplify-connective:expr/term op co no !iden y x)]
               [else (simplify-connective:expr/lit op co no !iden y x)])]
        [else #f]))

(define (simplify-connective:expr/term op co no !iden x y)
  (match x 
    [(expression (== no) (== y)) !iden]
    [(expression (== co) _ ... (== y) _ ...) y]
    [(expression (== op) _ ... (== y) _ ...) x]
    [(expression (== op) _ ... (expression (== no) (== y)) _ ...) !iden]
    [(expression (== no) (expression (== co) _ ... (== y) _ ...)) !iden]
    [(expression (== no) (expression (== co) _ ... (expression (== no) (== y)) _ ...)) x]
    [(expression (== no) (expression (== op) _ ... (expression (== no) (== y)) _ ...)) y]
    [(expression (== no) a) 
     (match y 
       [(expression (== op) _ ... (== a) _ ...) !iden]
       [_ #f])]
    [_ #f]))

(define (simplify-connective:expr/lit op co no !iden x y)
  (define !y ((op-unsafe no) y))
  (match x 
    [(expression (== co) (== y) _ ...) y]
    [(expression (== op) (== y) _ ...) x]
    [(expression (== op) (== !y) _ ...) !iden]
    [(expression (== no) (expression (== co) (== y) _ ...)) !iden]
    [(expression (== no) (expression (== co) (== !y) _ ...)) x]
    [(expression (== no) (expression (== op) (== !y) _ ...)) y]
    [_ #f]))

; Simplification rules for bitwise and/or, applied to fix point. 
; Assumes that the xs list contains no literals, only terms.
(define (simplify-connective* op co no literal? !iden xs)
  (or
   (let-values ([(!ys ys) (for/fold ([!ys '()][ys '()]) ([x xs])
                            (match x
                              [(expression (== no) y) (values (cons y !ys) ys)]
                              [_ (values !ys (cons x ys))]))])
     (for/first ([!y !ys] #:when (member !y ys)) (list !iden)))
   (and (> (length xs) 100) xs)
   (let outer ([xs xs])
     (match xs
       [(list x rest ..1)
        (let inner ([head rest] [tail '()])
          (match head
            [(list) (match (outer tail)
                      [(and (list (== !iden)) t) t]
                      [t (cons x t)])]
            [(list y ys ...)
             (match (simplify-connective op co no literal? !iden x y)
               [#f (inner ys (cons y tail))]
               [(== !iden) (list !iden)]
               [v (outer (cons v (append ys tail)))])]))]
       [_ xs]))))   



(define-syntax-rule (define-not not-op not-symbol term-type racket-type racket-op)
  (define-op not-op 
    #:name not-symbol
    #:type (op/-> (term-type) term-type)
    #:op   (lambda (x)
             (if (racket-type x)
                 (racket-op x)
                 (if (equal? not-op (term-op x))
                     (first (term-child x))
                     (expression not-op x))))))

(define-syntax-rule (define-and and-op and-symbol or-op not-op T term-type racket-type racket-op)
  (define-op and-op 
    #:name and-symbol
    #:type (op/-> (#:rest term-type) term-type) 
    #:op   (case-lambda [(a b) (binary:and/or and-op or-op not-op racket-type racket-op T a b)]
                        [args (nary:and/or and-op or-op not-op racket-op T args)])))

(define-syntax-rule (define-or or-op or-symbol and-op not-op F term-type racket-type racket-op)
  (define-op or-op 
    #:name or-symbol
    #:type (op/-> (#:rest term-type) term-type) 
    #:op   (case-lambda [(a b) (binary:and/or or-op and-op not-op racket-type racket-op F a b)]
                        [args (nary:and/or or-op and-op not-op racket-op F args)])))

; Applies the binary and/or  operator to the given arguments.
(define (binary:and/or op co not-op racket-type racket-op identity a b)
  (if (and (racket-type a) (racket-type b))
      (racket-op a b)
      (begin-value
        (simplify:and/or op co not-op identity (not-op identity) a b)
        (if (term? a)
            (if (term? b)
                (if (term<? a b) (expression op a b) (expression op b a))
                (expression op b a))
            (expression op a b)))))
     
; Applies the nary and/or operator to the given arguments.
(define (nary:and/or op co neg racket-op identity args)
  
  (define annihilator (neg identity))
  
  (define (simplify args)
    (if (> (length args) 100)
        args
        (let ([simplified (simplify:and/or:pairwise op co neg identity annihilator args)])
          (if (equal? simplified args) args (simplify simplified)))))
  
  (let*-values ([(syms vals) (partition term? (remove-duplicates args))]
                [(val) (apply racket-op vals)])
    (if (equal? val annihilator)
        annihilator
        (match (simplify syms)
          [(list) val]
          [(list (== annihilator)) annihilator]
          [(list s) (if (equal? identity val) s (expression op val s))]
          [xs (if (equal? identity val)
                  (apply expression op (sort xs term<?))
                  (apply expression op (cons val (sort xs term<?))))]))))

; Applies simplify:and/or pairwise, unless it encounters the annihilator value, 
; in which case it aborts the computation and returns.
(define (simplify:and/or:pairwise op co not-op identity annihilator args)
  (match args
    [(list x rest ..1)
     (let loop ([xs rest] [simp '()] [simp? #f])
       (match xs 
         [(list) 
          (if simp?
              (simplify:and/or:pairwise op co not-op identity annihilator (reverse simp))
              (cons x (simplify:and/or:pairwise op co not-op identity annihilator rest)))]
         [(list y ys ...)
          (match (simplify:and/or op co not-op identity annihilator x y)
            [(== NaV) (loop ys (cons y simp) simp?)]
            [(== annihilator) (list annihilator)]
            [v (loop ys (cons v simp) #t)])]))]
    [_ args]))

; Applies the basic and/or simplifications to its arguments.  The simplifications 
; are chosen so that they never result in creation of additional expressions, other 
; than (op a b) itself. Returns NaV if none of the rules applicable; otherwise returns 
; the simplified result.
(define (simplify:and/or op co not-op identity annihilator a b)
  (begin-value 
    (simplify-1 op co not-op identity annihilator a b)
    (simplify-2 op co not-op annihilator a b)))


(define (cancel? not-op a b)
  (match* (a b)
    [(_ (expression (== not-op) (== a))) #t]
    [((expression (== not-op) (== b)) _) #t]
    [(_ _) #f]))

; Applies basic logic laws (commutativity, identity, annihilation, absorption).
; Returns NaV if none of the rules applicable; otherwise returns the simplified result.
(define (simplify-1 op co neg identity annihilator a b)
  (cond [(equal? a b) a]
        [(expression? a)
         (if (expression? b)
             (begin-value 
               (simplify-1:expr/any op co neg annihilator a b)
               (simplify-1:expr/any op co neg annihilator b a)
               (simplify-1:expr/expr op co neg annihilator a b))
             (begin-value
               (simplify-1:any/any identity annihilator b a)
               (simplify-1:expr/any op co neg annihilator a b)))]
        [(expression? b)
         (begin-value
           (simplify-1:any/any identity annihilator a b)
           (simplify-1:expr/any op co neg annihilator b a))]
        [else
         (begin-value
           (simplify-1:any/any identity annihilator a b)
           (simplify-1:any/any identity annihilator b a))]))

(define (simplify-1:any/any identity annihilator a b)
  (match a
    [(== identity) b]
    [(== annihilator) annihilator]
    [_ NaV]))
    
(define (simplify-1:expr/any op co neg annihilator a b)
  (match a 
    [(expression (== neg) (== b)) annihilator]
    [(expression (== co) _ ... (== b) _ ...) b]
    [(expression (== op) _ ... (== b) _ ...) a]
    [(expression (== op) _ ... (expression (== neg) (== b)) _ ...) annihilator]
    [(expression (== neg) (expression (== co) _ ... (== b) _ ...)) annihilator]
    [(expression (== neg) (expression (== co) _ ... (expression (== neg) (== b)) _ ...)) a]
    [(expression (== neg) (expression (== op) _ ... (expression (== neg) (== b)) _ ...)) b]
    [_ NaV]))

(define (simplify-1:expr/expr op co neg annihilator a b)
  (match* (a b)
    [((expression (== op) xs ...) (expression (== neg) y)) (if (member y xs) annihilator NaV)]
    [((expression (== neg) y) (expression (== op) xs ...)) (if (member y xs) annihilator NaV)]
    [(_ _) NaV]))

; Applies the following simplification rules symmetrically:
; (1) (op (op a1 ... an) (op ai ... aj)) ==> (op a1 ... an)
; (2) (op (op a1 ... ai ... an) (op b1 ... (neg ai) ... bn) ==> annihilator
; (3) (op (co a1 ... an) (co ai ... aj)) ==> (co ai ... aj)
; Returns NaV if none of the rules applicable; otherwise returns the simplified result.
(define (simplify-2 op co neg annihilator a b)
  (match* (a b)
    [((expression (== op) xs ...) (expression (== op) ys ...))
     (cond [(sublist? xs ys) b]
           [(sublist? ys xs) a]
           [(ormap (lambda (x) (member (neg x) ys)) xs) annihilator]
           [else NaV])]
    [((expression (== co) xs ...) (expression (== co) ys ...))
     (cond [(sublist? xs ys) a]
           [(sublist? ys xs) b]
           [else NaV])]
    [(_ _) NaV]))

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

(define NaV (new (class object% (super-new))))
(define (NaV? v) (eq? v NaV))
(define-syntax begin-value
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (let ([v e0])
                    (if (NaV? v)
                        (begin-value e ...)
                        v))]))

