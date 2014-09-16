#lang racket

(require "env.rkt" (prefix-in kks/ (only-in "kks.rkt" not and or => <=> ite =))
         (except-in "kks.rkt" configure not and or => <=> ite =) "univ.rkt"
         "../common/enc.rkt"
         "../../base/term.rkt" "../../base/generic.rkt" 
         "../../base/op.rkt" "../../base/num.rkt" "../../base/bool.rkt" "../../base/enum.rkt")

(provide enc)
    
; The enc procedure takes a value and an environment, and returns  
; a Kodkod encoding of that value in the given environment.  If it 
; cannot produce an encoding for the given value, an error is thrown. 
; The environment will be modified (if needed) to include an encoding for 
; the given value and all of its subexpressions (if any).
(define (enc v env)
  (ref! env v (match v
                [(? expression?) (enc-expr v env)]
                [(? constant?)  (enc-const v env)]
                [_             (enc-lit v env)])))

(define (enc-expr v env)
  (match v
    [(expression (== @*) -1 e) 
     (bvneg (enc e env))]
    [(expression (== @*) (and (? rational?) (not (? integer?)) r) es ...) 
     (bvsdiv (apply bvmul (enc (numerator r) env) (for/list ([e es]) (enc e env))) 
             (enc (denominator r) env))]
    [(expression (== @*) a ... (expression (== @expt) x (and (? number?) (? negative? n))) b ...)
     (let ([a/x^n (bvsdiv (enc (apply @* a) env) (enc (@expt x (- n)) env))])
       (if (null? b) a/x^n (bvmul a/x^n (enc (apply @* b) env))))]
    [(expression (== @expt) e (? integer? n)) 
     (let ([e^n (apply bvmul (make-list (abs n) (enc e env)))])
       (if (< n 0) (bvsdiv 1 e^n) e^n))]
    [(expression (app rosette->kodkod (? procedure? kks/op)) es ...)
     (apply kks/op (for/list ([e es]) (enc e env)))]
    [(expression (? enum-comparison-op? op) x y) 
     (in (-> (enc x env) (enc y env)) (ref! env op #:bound (enum-order-bound (type-of x) env)))]
    [_ (error 'encode "cannot encode expression ~a" v)]))
    
(define (enc-const v env)
  (match (term-type v)
    [(== @boolean?) (some (ref! env v #:bound NONE (tupleset (tuple 0))))]
    [(== @number?)  (sum  (ref! env v #:bound NONE INTS))]
    [(? enum? t)       (ref! env v #:bound NONE (enum-bound t env) #:invariant (enc (scalar v) env))]
    [_ (error 'enc-constant "expected a boolean?, number? or enum?, given ~a" v)]))

(define (enc-lit v env)
  (match v 
    [#t TRUE]
    [#f FALSE]
    [(? number?) (finitize v)]
    [(? enum-literal?) (ref! env v #:bound (enum-literal-bound v env))]
    [_ (error 'enc-literal "expected a boolean?, number? or enum-literal?, given ~a" v)]))
 
(define (enum-literal-bound v env)
  (let ([dom (domain-of (univ env) (type-of v))]) 
    (tupleset (tuple (domain-atom dom v)))))

(define (enum-bound enum env)
  (let ([dom (domain-of (univ env) enum)])
    (tupleset #:range (tuple (domain-min dom)) (tuple (domain-max dom)))))

(define (enum-order-bound enum env)
  (let ([dom (domain-of (univ env) enum)])
    (tupleset #:tuples 
              (for*/list ([i (in-range (domain-min dom) (domain-max dom))]
                          [j (in-range (add1 i) (add1 (domain-max dom)))])
                (tuple i j)))))
  
(define-op scalar ; A predicate operator indicating that the relational encoding  
  #:name 'scalar  ; of a given variable must be a scalar (singleton unary relation).
  #:type (op/-> [constant?] @boolean?)
  #:op   (lambda (val) (expression scalar val)))

(define-encoder rosette->kodkod  
  [! kks/not] [&& kks/and] [|| kks/or] [<=> kks/<=>] [=> kks/=>] 
  [ite kks/ite] [=? kks/=] [@= kks/=] [@< bvslt] [@<= bvsle] 
  [@bitwise-ior bvor] [@bitwise-and bvand] 
  [@bitwise-not bvnot] [@bitwise-xor bvxor]
  [@<< bvshl] [@>>> bvlshr] [@>> bvashr]
  [@+ bvadd] [@* bvmul] [@quotient bvsdiv] [@remainder bvsrem] 
  [@abs bvabs] [@sgn bvsgn] [scalar one])
