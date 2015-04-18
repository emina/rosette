#lang racket

(require (prefix-in smt/ (only-in "smtlib2.rkt" not and or xor => <=> ite = <))
         (except-in "smtlib2.rkt" not and or xor => <=> ite = <) "env.rkt" 
         "../common/enc.rkt" "../../base/term.rkt" 
         "../../base/generic.rkt" "../../base/num.rkt" "../../base/bool.rkt"
         "../../base/enum.rkt")

(provide enc finitize)

; The enc procedure takes a value and an environment, and returns  
; an SMTLIB identifier representing that value in the given environment.  If it 
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
    [(expression (app rosette->smt (? procedure? smt/op)) es ...)
     (apply smt/op (for/list ([e es]) (enc e env)))]
    [(expression (== @*h) x y)
     (extract (sub1 (* 2 (current-bitwidth))) 
              (current-bitwidth)
              (bvmul (concat (enc 0 env) (enc x env)) 
                     (concat (enc 0 env) (enc y env))))]
    [_ (error 'encode "cannot encode expression ~a" v)]))

(define (enc-const v env)
  (ref! env v))

(define (enc-lit v env)
  (match v 
    [#t true]
    [#f false]
    [(? number?) (bv (finitize v) (current-bitwidth))]
    [(? enum-literal?) (ordinal v)]
    [_ (error 'enc-literal "expected a boolean?, number? or enum-literal?, given ~a" v)]))

(define-encoder rosette->smt 
  [#:== [! smt/not] [&& smt/and] [|| smt/or] [=> smt/=>] [<=> smt/<=>]  
        [ite smt/ite] [=? smt/=] [@= smt/=] [@< bvslt] [@<= bvsle] 
        [@bitwise-ior bvor] [@bitwise-and bvand] 
        [@bitwise-not bvnot] [@bitwise-xor bvxor]
        [@<< bvshl] [@>>> bvlshr] [@>> bvashr]
        [@+ bvadd] [@* bvmul] [@quotient bvsdiv] [@remainder bvsrem]
        [@abs smt/abs] [@sgn smt/sgn]]
  [#:?  [enum-comparison-op? smt/<]])

(define (smt/abs e)
  (smt/ite (bvslt e (bv 0 (current-bitwidth))) (bvneg e) e))

(define (smt/sgn e)
  (let ([zero (bv 0 (current-bitwidth))]) 
    (smt/ite (smt/= e zero) zero 
             (smt/ite (bvslt e zero) 
                      (bv -1 (current-bitwidth))
                      (bv  1 (current-bitwidth))))))
