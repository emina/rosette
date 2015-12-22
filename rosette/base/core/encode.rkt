#lang racket

(require racket/syntax 
         "term.rkt" "real.rkt" "bitvector.rkt" "bool.rkt" 
         "polymorphic.rkt" "merge.rkt")

(provide encode-to-bv)

; The encode-to-bv procedure takes as input a list of terms, in any logic, 
; and encodes those terms in bitvector logic, representing integers and reals 
; as bitvectors of given length (current-bitwidth by default).  The output is 
; a list of terms in QF_BV.  Each new constant in the output that represents 
; a constant in the input is annotated with that constant.  That is, 
; (term-property o 'source) applied to a freshly created constant o returns 
; the constant i that is represented by o.  If (term-property o 'source) returns 
; #f, then the constant represents itself (i.e., it occurred in the input terms).
(define (encode-to-bv terms [bw (current-bitwidth)])
  (define env (make-hash))
  (for/list ([t terms])
    (enc t env)))

; The enc procedure takes a value (a term or a literal), 
; and an environment (a hash-map from terms to their QF_BV encoding), and returns  
; a QF_BV term representing that value in the given environment.  If it 
; cannot produce an encoding for the given value, an error is thrown. 
; The environment will be modified (if needed) to include an encoding for 
; the given value and all of its subexpressions (if any).
(define (enc v env)
  (or (hash-ref env v #f)
      (hash-ref! env v 
                 (match v
                   [(? expression?) (enc-expr v env)]
                   [(? constant?)   (enc-const v env)]
                   [_               (enc-lit v env)]))))

; TODO:  use unsafe ops for encoding.
(define (enc-expr v env)
  (term-properties v 
   (match v
     [(expression (== @=) x y)         (@bveq (enc x env) (enc y env))]
     [(expression (== @<) x y)         (@bvslt (enc x env) (enc y env))]
     [(expression (== @<=) x y)        (@bvsle (enc x env) (enc y env))]
     [(expression (== @-) x)           (@bvneg (enc x env))]
     [(expression (== @+) xs ...)      (apply @bvadd (for/list ([x xs]) (enc x env)))]
     [(expression (== @*) xs ...)      (apply @bvmul (for/list ([x xs]) (enc x env)))]
     [(expression (== @/) x y)         (@bvsdiv (enc x env) (enc y env))]
     [(expression (== @quotient) x y)  (@bvsdiv (enc x env) (enc y env))]
     [(expression (== @remainder) x y) (@bvsrem (enc x env) (enc y env))]
     [(expression (== @modulo) x y)    (@bvsmod (enc x env) (enc y env))]
     [(expression (== @int?) _)        #t]
     [(expression (== @abs) x) 
      (let ([e (enc x env)])
        (merge (@bvslt e (bv 0 (get-type e))) (@bvneg e) e))]
     [(expression (or (== @integer->real) (== @real->integer)) x _) 
      (enc x env)]
     [(expression (== @integer->bitvector) v (bitvector sz))
      (convert (enc v env) (current-bitwidth) sz @sign-extend)]
     [(expression (== @bitvector->natural) (and v (app get-type (bitvector sz))))
      (convert (enc v env) sz (current-bitwidth) @zero-extend)]
     [(expression (== @bitvector->integer) (and v (app get-type (bitvector sz))))
      (convert (enc v env) sz (current-bitwidth) @sign-extend)]
     [(expression (== ite) a b c)
      (merge (enc a env) (enc b env) (enc c env))]
     [(expression op x)     
      (op (enc x env))]
     [(expression op x y)   
      (op (enc x env) (enc y env))]
     [(expression op xs ...) 
      (apply op (for/list ([x xs]) (enc x env)))])))
    
(define (enc-const v env)
  (match v
    [(constant (or id (cons id _)) (or (== @integer?) (== @real?)))
     (term-property 
      (term-properties v 
       (constant (format-id id "~a" (gensym (term-e v)) #:source id)
                 (bitvector (current-bitwidth))))
      'source v)]
    [_ v]))
                              
(define (enc-lit v env)
  (match v 
    [(? real?) (bv v (current-bitwidth))]
    [_ v]))

(define (convert v src tgt @extend)
  (cond [(= src tgt) v]
        [(> src tgt) (@extract (- tgt 1) 0 v)]
        [else        (@extend v (bitvector tgt))]))

