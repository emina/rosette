#lang racket

(require "../base/core/term.rkt" "../base/core/bool.rkt" 
         "../base/core/polymorphic.rkt" "../base/core/union.rkt"
         "../base/core/merge.rkt" "../solver/solution.rkt")

(provide evaluate)

; Partially evaluates the given expression with respect to the provided solution and 
; returns the result.  In particular, if the solution has a binding for every symbolic 
; variable occuring in the expression, the output is a concrete value.  Otherwise, the 
; output is a (possibly) symbolic value, expressed in terms of variables that are not
; bound by the given solution.  The solution must be sat?.
(define (evaluate expr sol)
  (if (and (sat? sol) (= (dict-count (model sol)) 0)) 
      expr
      (eval-rec expr sol (make-hash))))

(define (eval-rec expr sol cache)
  (if (hash-has-key? cache expr) 
      (hash-ref cache expr)
      (let ([result
             (match expr
               [(? constant?) (sol expr)]
               [(expression (== ite) b t f) 
                (match (eval-rec b sol cache)
                  [#t (eval-rec t sol cache)]
                  [#f (eval-rec f sol cache)]
                  [g (ite g (eval-rec t sol cache) (eval-rec f sol cache))])]
               [(or (expression (== ite*) gvs ...) (union gvs))
                (if (union? expr)
                    (eval-guarded gvs sol cache car cdr)
                    (eval-guarded gvs sol cache guarded-test guarded-value))]
               [(expression (and op (or (== @forall) (== @exists))) vars body)
                ((operator-unsafe op)
                 vars
                 (eval-rec
                  body
                  (sat (for/hash ([(k v) (model sol)] #:unless (member k vars)) (values k v)))
                  (make-hash)))]
               [(expression op child ...)  
                (apply (operator-unsafe op) (for/list ([e child]) (eval-rec e sol cache)))]
               [(? list?)                
                (for/list ([e expr]) (eval-rec e sol cache))]
               [(cons x y)               
                (cons (eval-rec x sol cache) (eval-rec y sol cache))]
               [(? vector?)              
                (for/vector #:length (vector-length expr) ([e expr]) (eval-rec e sol cache))]
               [(? box?)
                ((if (immutable? expr) box-immutable box) (eval-rec (unbox expr) sol cache))]
               [(? typed?)              
                (let ([t (get-type expr)])
                  (match (type-deconstruct t expr)
                    [(list (== expr)) expr]
                    [vs (type-construct t (for/list ([v vs]) (eval-rec v sol cache)))]))]
               [_ expr])])
        (hash-set! cache expr result)
        result)))

(define (eval-guarded gvs sol cache test value)
  (let loop ([vs gvs] [out '()])
    (if (null? vs) 
        (apply merge* out)
        (let ([gv (car vs)])
          (match (eval-rec (test gv) sol cache)
            [#t (eval-rec (value gv) sol cache)]
            [#f (loop (cdr vs) out)]
            [g  (loop (cdr vs) (cons (cons g (eval-rec (value gv) sol cache)) out))])))))
  
