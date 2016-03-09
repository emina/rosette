#lang racket

(require racket/syntax 
         "../base/core/term.rkt"
         "../base/core/real.rkt"
         "../base/core/bitvector.rkt"  
         "../base/core/polymorphic.rkt"
         "../base/core/merge.rkt"
         "../base/core/union.rkt"
         (only-in "../base/core/term.rkt" [function-unsafe unsafe]))

(provide finitize current-bitwidth)

; The current bitwidth parameter controls the finitization of real / integer terms.
(define current-bitwidth
  (make-parameter 5 
                  (lambda (bw) 
                    (unless (or (false? bw) (and (integer? bw) (positive? bw)))
                      (raise-argument-error 'current-bitwidth "positive integer or #f" bw))
                    bw)))

; The finitize procedure takes as input a list of terms, in any combination of theories, 
; and encodes those terms in the theory of bitvectors (BV), representing integers and reals 
; as bitvectors of length bw.  The bw parameter is optional and defaults to current-bitwidth.  
; The optional map argument must be a mutable hash map, which is either empty or was 
; returned by a previous call to finitize with the same value of bw.
; 
; The procedure updates and returns the given map, which binds the given input terms, and 
; their subterms, to their corresponding BV finitizations.  Terms that are already in BV 
; finitize to themselves.
(define (finitize terms [bw (current-bitwidth)] [env (make-hash)])
  (parameterize ([current-bitwidth bw])
    (for ([t terms])
      (enc t env))
    env))

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

(define (enc-expr v env)
  (match v
    [(expression (== @=) x y)         ((unsafe @bveq) (enc x env) (enc y env))]
    [(expression (== @<) x y)         ((unsafe @bvslt) (enc x env) (enc y env))]
    [(expression (== @<=) x y)        ((unsafe @bvsle) (enc x env) (enc y env))]
    [(expression (== @-) x)           ((unsafe @bvneg) (enc x env))]
    [(expression (== @+) xs ...)      (apply (unsafe @bvadd) (for/list ([x xs]) (enc x env)))]
    [(expression (== @*) xs ...)      (apply (unsafe @bvmul) (for/list ([x xs]) (enc x env)))]
    [(expression (== @/) x y)         ((unsafe @bvsdiv) (enc x env) (enc y env))]
    [(expression (== @quotient) x y)  ((unsafe @bvsdiv) (enc x env) (enc y env))]
    [(expression (== @remainder) x y) ((unsafe @bvsrem) (enc x env) (enc y env))]
    [(expression (== @modulo) x y)    ((unsafe @bvsmod) (enc x env) (enc y env))]
    [(expression (== @int?) _)        #t]
    [(expression (== @abs) x) 
     (let ([e (enc x env)])
       (merge ((unsafe @bvslt) e (bv 0 (get-type e))) ((unsafe @bvneg) e) e))]
    [(expression (or (== @integer->real) (== @real->integer)) x) 
     (enc x env)]
    [(expression (== @integer->bitvector) v (bitvector sz))
     (convert (enc v env) (current-bitwidth) sz @sign-extend)]
    [(expression (== @bitvector->natural) v)
     (convert (enc v env) (bitvector-size (get-type v)) (current-bitwidth) @zero-extend)]
    [(expression (== @bitvector->integer) v)
     (convert (enc v env) (bitvector-size (get-type v)) (current-bitwidth) @sign-extend)]
    [(expression (== @extract) i j x) ; i and j must be ints
     ((unsafe @extract) i j (enc x env))]
    [(expression (and op (or (== @sign-extend) (== @zero-extend))) x t) ; t must be bitvector? type
     ((unsafe op) (enc x env) t)]
    [(expression (== ite) a b c)
     (merge (enc a env) (enc b env) (enc c env))]
    ((expression (== ite*) gvs ...)
     (let ([e (apply merge* 
                     (for/list ([gv gvs]) 
                       (cons (enc (guarded-test gv) env) (enc (guarded-value gv) env))))])
       (match e 
         [(union) (error 'finitize "all finitizations infeasible: ~a" v)]
         [_ e])))
    [(expression op x)     
     ((unsafe op) (enc x env))]
    [(expression op x y)   
     ((unsafe op) (enc x env) (enc y env))]
    [(expression op xs ...) 
     (apply (unsafe op) (for/list ([x xs]) (enc x env)))]))
    
(define (enc-const v env)
  (match v
    [(constant id (or (== @integer?) (== @real?)))
     (constant ((if (list? id) cons list) (gensym 'bv) id) (bitvector (current-bitwidth)))] 
    [_ v]))
                              
(define (enc-lit v env)
  (match v 
    [(? real?) (bv v (current-bitwidth))]
    [_ v]))

(define (convert v src tgt @extend)
  (cond [(= src tgt) v]
        [(> src tgt) ((unsafe @extract) (- tgt 1) 0 v)]
        [else        ((unsafe @extend) v (bitvector tgt))]))
