#lang racket

(require (only-in "smtlib2.rkt" assert minimize maximize
                  check-sat get-model get-unsat-core
                  read-solution true false)
         "env.rkt" "enc.rkt"
         (only-in "../../base/core/term.rkt" constant? term-type solvable-default type-of)
         (only-in "../../base/core/function.rkt" fv function? function-domain function-range)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         "../solution.rkt")

(provide encode encode-for-proof decode)

; Given an encoding environment and a list of asserts, minimization objectives,
; and maximization objective, the encode procedure prints an SMT encoding of the given assertions, 
; with respect to the given environment, to current-output-port. 
; In particular, the encoder will not emit any declarations or definitions for Rosette 
; values that appear in the given assertions and that are 
; already bound in the environment.  The environment will 
; be augmented, if needed, with additional declarations and 
; definitions.
(define (encode env asserts mins maxs)
  (for ([a asserts])
    (assert (enc a env)))
  (for ([m mins])
    (minimize (enc m env)))
  (for ([m maxs])
    (maximize (enc m env)))
  (check-sat)
  (get-model))

; Given an encoding environment and a list of asserts, 
; the encode-labeled procedure prints an SMT encoding of the given assertions 
; to current-output-port, ensuring that each printed assertion is named. 
; This procedure expects the assertions to be unsatifisable, and the underlying 
; solver's unsat-core-extraction option to be set.  The environment will be augmented, 
; if needed, with additional declarations and definitions.
(define (encode-for-proof env asserts)
  (for ([a asserts])
    (define id (enc a env))
    (assert id (id->name id)))
  (check-sat)
  (get-unsat-core))

; Generates an assertion label for a declared or defined SMT id by prefixing that 
; id with 'a'.  This will generate unique ids, since none of the declared or defined 
; constants start with 'a' (see env.rkt).
(define (id->name id)
  (string->symbol (format "a~a" (symbol->string id))))

; Converts an assertion label (produced by id->name) to a declared or defined SMT id 
; by stripping off the first character ('a').
(define (name->id name)
  (string->symbol (substring (symbol->string name) 1)))

; Given an encoding enviornment, the decode procedure reads 
; the solution from current-input-port and converts it into a 
; Rosette solution object.  The port must be connected to a 
; solver working on a problem P such that every identifier 
; declared or defined in P is bound in (decls env) or (defs env), respectively.
(define (decode env)
  (match (read-solution)
    [(cons (? hash? sol) (? hash? objs))
     (sat (for/hash ([(decl id) (in-dict (decls env))])
            (let ([t (term-type decl)])
              (values decl
                      (if (hash-has-key? sol id)
                          (if (function? t)
                              (decode-function t (hash-ref sol id))
                              (decode-value t (hash-ref sol id)))
                          (solvable-default t)))))
          (if (hash-empty? objs)
              (hash)
              (for/hash ([(obj id) (in-sequences (in-dict (decls env)) (in-dict (defs env)))]
                         #:when (hash-has-key? objs id))
                (values obj
                        (decode-objective (type-of obj) (hash-ref objs id))))))]                
    [(? list? names)
     (unsat (let ([core (apply set (map name->id names))])
              (for/list ([(bool id) (in-sequences (in-dict (decls env)) (in-dict (defs env)))]
                          #:when (set-member? core id)) 
                 bool)))]
    [#f (unsat)]))


(define (to-exact-int a) (if (integer? a) (inexact->exact a) a))

(define (decode-value type val)
  (match type
    [(== @boolean?)
     (match val
       [(== true) #t]
       [(== false) #f])]
    [(== @integer?) 
     (match val
       [(? integer?) val]
       [(list '- v) (- v)])]
    [(== @real?) 
     (match val 
       [(? real?) val]
       [(list '- (list '/ a b)) (- (/ (to-exact-int a) (to-exact-int b)))]
       [(list '- v) (- v)]
       [(list '/ a b) (/ (to-exact-int a) (to-exact-int b))]
       [(list '/ (list '- a) b) (/ (- (to-exact-int a)) (to-exact-int b))])]
    [(? bitvector? t)
     (match val
       [(? number?) (bv val t)]
       [(list _ (app symbol->string (regexp #px"bv(\\d+)" (list _ (app string->number n)))) _)
        (bv n t)])]
    [other other]))

(define (decode-function type val)
  (define default+tbl
    (reverse
     (match val
      [(list (== 'define-fun) _ params _ body)
       (decode-body (map car params) (function-domain type) (function-range type) body)])))
  (fv (reverse (cdr default+tbl)) (car default+tbl) type))
  
(define (decode-body params dom ran body)
  (match body
    [(list (== 'ite) args v0 (and rest (list (== 'ite) _ ...)))
     (cons (cons (decode-args params dom args) (decode-value ran v0))
           (decode-body params dom ran rest))]
    [(list (== 'ite) args v0 v1)
     (list (cons (decode-args params dom args) (decode-value ran v0))
           (decode-value ran v1))]))

(define (decode-args params types args)
  (for/list ([p params][t types])
    (decode-value
     t 
     (match args
       [(list (== 'and) _ ... (list (== '=) (== p) y) _ ...) y]
       [(list (== 'and) _ ... (list (== '=) x (== p)) _ ...) x]
       [(list (== '=) x y)
        (or (and (equal? p x) y)
            (and (equal? p y) x))]))))
  
(define (decode-objective type expr)
  (match expr
    [(app decode-objective-value (cons val ε))
     (if (bitvector? type)
         (interval (bv val type) #t (bv val type) #t)
         (cond [(zero? ε) (interval val #t val #t)]
               [(positive? ε) (interval val #f +inf.0 #f)]
               [else (interval -inf.0 #f val #f)]))]
    [(list (app decode-objective-value (cons min-val minε))
           (app decode-objective-value (cons max-val maxε)))
     (if (bitvector? type)
         (interval (bv min-val type) #t (bv max-val type) #t)
         (interval min-val (zero? minε) max-val (zero? maxε)))]))

(define (decode-objective-value expr)
  (match (normalize-objective-value expr)
    [(? real? r) (cons r 0)]
    [(== 'epsilon) (cons 0 1)]
    [(list (== '*) (? real? r) (== 'epsilon)) (cons 0 r)]
    [(list (== '+) (? real? r) (== 'epsilon)) (cons r 1)]
    [(list (== '+) (? real? r) (list (== '*) (? real? s) (== 'epsilon))) (cons r s)]
    [_ #f]))

(define (normalize-objective-value expr)
  (match expr
    [(or (? real?) (== 'epsilon)) expr]
    [(== 'oo) +inf.0]
    [(list (== 'to_real) r) r]
    [(list (== '-) r) (- r)]
    [(list (== '+) e0 e1)
     (match* ((normalize-objective-value e0) (normalize-objective-value e1))
       [((? real? r0) (? real? r1)) (+ r0 r1)]
       [((? real? r) other) `(+ ,r ,other)]
       [(other (? real? r)) `(+ ,r ,other)])]
    [(list (== '*) e0 e1)
     (match* ((normalize-objective-value e0) (normalize-objective-value e1))
       [((? real? r0) (? real? r1)) (* r0 r1)]
       [((? real? r) other) `(* ,r ,other)]
       [(other (? real? r)) `(* ,r ,other)])]
    [_ #f]))
    
    

  
     

  


