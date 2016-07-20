#lang racket

(require racket/syntax 
         "../base/core/term.rkt"
         "../base/core/bool.rkt"
         "../base/core/real.rkt"
         "../base/core/bitvector.rkt"
         "../base/core/function.rkt"  
         "../base/core/polymorphic.rkt"
         "../base/core/merge.rkt"
         "../base/core/union.rkt"
         (only-in "../solver/solution.rkt" model core sat unsat sat? unsat?)
         (only-in "../base/core/term.rkt" [operator-unsafe unsafe]))

(provide finitize current-bitwidth unfinitize complete)

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
      (finitize-any t env))
    env))

; Takes as input a solution and a finitization map 
; produced by calling the finitize procedure, 
; and returns a new solution that applies the inverse 
; of the given map to the provided solution.   
(define (unfinitize sol fmap) 
  (match sol
    [(model m)
     (sat (for/hash ([(k fk) fmap] #:when (dict-has-key? m fk))
            (let ([t (term-type k)])
              (values k (cond [(equal? k fk) (dict-ref m fk)]
                              [(function? t) (unfinitize-fun t (dict-ref m fk))]
                              [else (bv-value (dict-ref m fk))])))))]
    [(core #f) sol] ; no core extracted
    [(core φs) (unsat (for/list ([(k v) fmap] #:when (member v φs)) k))]))

(define (unfinitize-value t v)
  (if (infinite? t) (bv-value v) v))

(define (unfinitize-fun t fval)
  (match-define (fv ios o type) fval)
  (match-define (function dom ran) t)
  (fv (for/list ([io ios])
        (cons (for/list ([i (car io)][d dom])
                (unfinitize-value d i))
              (unfinitize-value ran (cdr io))))
      (unfinitize-value ran o)
      t))
       
                       
; Takes as input a solution and a finitization map 
; produced by calling the finitize procedure in finitize.rkt, 
; and returns a solution that is complete with respect to the given map.  
; That is, if the given solution is satisfiable but has no mapping for a
; constant in fmap, the returned solution has a default binding  
; for that constant (and same bindings as sol for other constants).  If 
; the given solution is unsat, it is simply returned.
(define (complete sol fmap) 
  (match sol
    [(model m)
     (sat (for/hash ([(k fk) fmap] #:when (constant? k))
            (values fk (if (dict-has-key? m fk)
                           (dict-ref m fk)
                           (solvable-default (term-type fk))))))]
    [_ sol]))

; The finitize-any procedure takes a value (a term or a literal), 
; and an environment (a hash-map from terms to their QF_BV encoding), and returns  
; a QF_BV term representing that value in the given environment.  If it 
; cannot produce an encoding for the given value, an error is thrown. 
; The environment will be modified (if needed) to include an encoding for 
; the given value and all of its subexpressions (if any).
(define (finitize-any v env)
  (hash-ref! env v
             (lambda ()
               (match v
                 [(? expression?)    (finitize-expr v env)]
                 [(? constant?)      (finitize-const v env)]
                 [_                  (finitize-lit v env)]))))

(define (finitize-expr v env)
  (match v
    [(expression (== @=) x y)         ((unsafe @bveq) (finitize-any x env) (finitize-any y env))]
    [(expression (== @<) x y)         ((unsafe @bvslt) (finitize-any x env) (finitize-any y env))]
    [(expression (== @<=) x y)        ((unsafe @bvsle) (finitize-any x env) (finitize-any y env))]
    [(expression (== @-) x)           ((unsafe @bvneg) (finitize-any x env))]
    [(expression (== @+) xs ...)      (apply (unsafe @bvadd) (for/list ([x xs]) (finitize-any x env)))]
    [(expression (== @*) xs ...)      (apply (unsafe @bvmul) (for/list ([x xs]) (finitize-any x env)))]
    [(expression (== @/) x y)         ((unsafe @bvsdiv) (finitize-any x env) (finitize-any y env))]
    [(expression (== @quotient) x y)  ((unsafe @bvsdiv) (finitize-any x env) (finitize-any y env))]
    [(expression (== @remainder) x y) ((unsafe @bvsrem) (finitize-any x env) (finitize-any y env))]
    [(expression (== @modulo) x y)    ((unsafe @bvsmod) (finitize-any x env) (finitize-any y env))]
    [(expression (== @int?) _)        #t]
    [(expression (== @abs) x) 
     (let ([e (finitize-any x env)])
       (merge ((unsafe @bvslt) e (bv 0 (get-type e))) ((unsafe @bvneg) e) e))]
    [(expression (or (== @integer->real) (== @real->integer)) x) 
     (finitize-any x env)]
    [(expression (== @integer->bitvector) v (bitvector sz))
     (convert (finitize-any v env) (current-bitwidth) sz @sign-extend)]
    [(expression (== @bitvector->natural) v)
     (convert (finitize-any v env) (bitvector-size (get-type v)) (current-bitwidth) @zero-extend)]
    [(expression (== @bitvector->integer) v)
     (convert (finitize-any v env) (bitvector-size (get-type v)) (current-bitwidth) @sign-extend)]
    [(expression (== @extract) i j x) ; i and j must be ints
     ((unsafe @extract) i j (finitize-any x env))]
    [(expression (and op (or (== @sign-extend) (== @zero-extend))) x t) ; t must be bitvector? type
     ((unsafe op) (finitize-any x env) t)]
    [(expression (== ite) a b c)
     (merge (finitize-any a env) (finitize-any b env) (finitize-any c env))]
    ((expression (== ite*) gvs ...)
     (let ([e (apply merge* 
                     (for/list ([gv gvs]) 
                       (cons (finitize-any (guarded-test gv) env)
                             (finitize-any (guarded-value gv) env))))])
       (match e 
         [(union) (error 'finitize "all finitizations infeasible: ~a" v)]
         [_ e])))
    [(expression (or (== @forall) (== @exists)) _ _)
     (error 'finitize "cannot use (current-bitwidth ~a) with a quantified formula ~a; use (current-bitwidth #f) instead"
            (current-bitwidth) v)]
    [(expression op x)     
     ((unsafe op) (finitize-any x env))]
    [(expression op x y)   
     ((unsafe op) (finitize-any x env) (finitize-any y env))]
    [(expression op xs ...) 
     (apply (unsafe op) (for/list ([x xs]) (finitize-any x env)))]))
    
(define (finitize-const v env)
  (match v
    [(constant id (? infinite?))
     (constant (finitize-identifier id) (bitvector (current-bitwidth)))]
    [(constant id (function dom ran))
     (if (or (infinite? ran) (ormap infinite? dom))
         (constant (finitize-identifier id) (function (map finitize-type dom) (finitize-type ran)))
         v)]
    [_ v]))
                              
(define (finitize-lit v env)
  (match v 
    [(? real?) (bv v (current-bitwidth))]
    [_ v]))
    
(define (convert v src tgt @extend)
  (cond [(= src tgt) v]
        [(> src tgt) ((unsafe @extract) (- tgt 1) 0 v)]
        [else        ((unsafe @extend) v (bitvector tgt))]))

(define (infinite? t)
  (or (equal? t @integer?) (equal? t @real?)))

(define (finitize-identifier id)
  ((if (list? id) cons list) (gensym 'bv) id))

(define (finitize-type t)
  (if (infinite? t)
      (bitvector (current-bitwidth))
      t))
