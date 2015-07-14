#lang racket

(require "state.rkt" "../config/log.rkt"
         "../base/term.rkt" "../base/bool.rkt" 
         "../base/generic.rkt" "../base/union.rkt"
         "../base/merge.rkt" "../solver/solution.rkt" )

(provide evaluate time/evaluate)

; Times the call to (evaluate expr sol), logs the timing 
; information and returns the result.
(define (time/evaluate expr sol)
  (let-values ([(val cpu real gc) (time-apply evaluate `(,expr ,sol))])
    (log-info "evaluation time (ms): cpu = ~a, real = ~a, gc = ~a" cpu real gc)
    (car val)))

; Partially evaluates the given expression with respect to the provided solution and 
; returns the result.  In particular, if the solution has a binding for every symbolic 
; variable occuring in the expression, the output is a concrete value.  Otherwise, the 
; output is a (possibly) symbolic value, expressed in terms of variables that are not
; bound by the given solution.  The solution must be sat?.
(define (evaluate expr [sol (current-solution)])
  (if (and (sat? sol) (= (dict-count (model sol)) 0)) 
      expr
      (eval-rec expr sol (make-hash))))

(define (eval-rec expr sol cache)
  (if (hash-has-key? cache expr) 
      (hash-ref cache expr)
      (let ([result
             (match expr
               [(? constant?)            
                (sol expr)]
               [(expression (== ite) b t f) 
                (match (eval-rec b sol cache)
                  [#t (eval-rec t sol cache)]
                  [#f (eval-rec f sol cache)]
                  [g (ite g (eval-rec t sol cache) (eval-rec f sol cache))])]
               [(expression (== @*) y (expression (== @expt) x -1))
                (finitize (@/ (finitize (eval-rec y sol cache)) (finitize (eval-rec x sol cache))))] 
               [(expression op child ...)  
                (finitize (apply op (for/list ([e child]) (finitize (eval-rec e sol cache)))))]
               [(? list?)                
                (for/list ([e expr]) (eval-rec e sol cache))]
               [(cons x y)               
                (cons (eval-rec x sol cache) (eval-rec y sol cache))]
               [(? vector?)              
                (for/vector #:length (vector-length expr) ([e expr]) (eval-rec e sol cache))]
               [(union vs)                 
                (let loop ([vs vs] [out '()])
                  (if (null? vs) 
                      (apply merge* out)
                      (let ([gv (car vs)])
                        (match (eval-rec (car gv) sol cache)
                          [#t (eval-rec (cdr gv) sol cache)]
                          [#f (loop (cdr vs) out)]
                          [g  (loop (cdr vs) (cons (cons g (eval-rec (cdr gv) sol cache)) out))]))))]
               [(? typed?)              
                (let ([t (get-type expr)])
                  (match (type-deconstruct t expr)
                    [(list (== expr)) expr]
                    [vs (type-construct t (for/list ([v vs]) (eval-rec v sol cache)))]))]
               [_ expr])])
        (hash-set! cache expr result)
        result)))

(require "../base/num.rkt")
(define (finitize num) 
  (match num
    [(? number? v) 
     (let* ([bitwidth (current-bitwidth)]
              [mask (arithmetic-shift -1 bitwidth)]
              [masked (bitwise-and (bitwise-not mask) (inexact->exact (truncate v)))])
         (if (bitwise-bit-set? masked (- bitwidth 1))
             (bitwise-ior mask masked)  
             masked))]
    [_ num]))
