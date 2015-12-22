#lang racket

(require (prefix-in smt/ (only-in "smtlib2.rkt" bv not and or xor => <=> ite = < <= + - * /))
         (only-in "smtlib2.rkt" [abs int_abs] div mod)
         (except-in "smtlib2.rkt" bv not and or xor => <=> ite = < + - * / abs) 
         "env.rkt" "../common/enc.rkt" 
         (only-in "../../base/core/term.rkt" expression expression? constant? get-type)
         (only-in "../../base/core/polymorphic.rkt" ite =?)
         (only-in "../../base/core/bool.rkt" ! && || => <=>)
         (only-in "../../base/core/num.rkt" 
                  current-bitwidth 
                  @= @< @<= @> @>= @+ @* @*h @- @/ @abs @sgn @quotient @remainder @expt                   
                  @<< @>> @>>> @bitwise-not @bitwise-and @bitwise-ior @bitwise-xor)
         (only-in "../../base/core/bitvector.rkt" 
                  bitvector? bv bitvector-size 
                  @bveq @bvslt @bvsle @bvult @bvule   
                  @bvnot @bvor @bvand @bvxor @bvshl @bvlshr @bvashr
                  @bvneg @bvadd @bvmul @bvudiv @bvsdiv @bvurem @bvsrem @bvsmod
                  @concat @extract @zero-extend @sign-extend 
                  @integer->bitvector @bitvector->integer @bitvector->natural)
         (prefix-in $ (only-in "../../base/core/real.rkt" @integer? @real? @= @< @<= @>= @> 
                               @+ @* @- @/ @quotient @remainder @modulo 
                               @abs @integer->real @real->integer @int?))
         (only-in "../../base/struct/enum.rkt" enum-literal? ordinal))

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
    [(or (expression (and op (== @integer->bitvector)) v (? bitvector? (app bitvector-size sz)))
         (expression (and op (== @bitvector->integer)) 
                     (and v (app get-type (? bitvector?  (app bitvector-size sz)))))
         (expression (and op (== @bitvector->natural)) 
                     (and v (app get-type (? bitvector? (app bitvector-size sz))))))
     (let-values ([(src tgt) (if (equal? op @integer->bitvector) (values (current-bitwidth) sz) (values sz (current-bitwidth)))]
                  [(extend)  (if (equal? op @bitvector->natural) zero_extend sign_extend)])
       (cond [(= src tgt) (enc v env)]
             [(> src tgt) (extract (- tgt 1) 0 (enc v env))]
             [else        (extend (- tgt src) (enc v env))]))]
    [(expression (== @extract) i j e)
     (extract i j (enc e env))]
    [(expression (== @zero-extend) v t)
     (zero_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @sign-extend) v t)
     (sign_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @*h) x y)
     (extract (sub1 (* 2 (current-bitwidth))) 
              (current-bitwidth)
              (bvmul (concat (enc 0 env) (enc x env)) 
                     (concat (enc 0 env) (enc y env))))]
    [(expression (== $@abs) e)
     (let ([te (enc e env)])
       (if (equal? (get-type v) $@integer?) 
           (int_abs te) 
           (smt/ite (smt/< te 0) (smt/- te) te)))]
    [(expression (== $@quotient) x y) 
     (let* ([tx (enc x env)]
            [ty (enc y env)]
            [tx/ty (div (int_abs tx) (int_abs ty))])
       (smt/ite (smt/= (smt/< tx 0) (smt/< ty 0)) tx/ty (smt/- tx/ty)))]   
    [(expression (== $@remainder) x y) 
     (let* ([tx (enc x env)]
            [ty (enc y env)]
            [tx%ty (mod (int_abs tx) (int_abs ty))])
       (smt/ite (smt/< tx 0) (smt/- tx%ty) tx%ty))]
    [(expression (== $@modulo) x y) 
     (let* ([tx (enc x env)]
            [ty (enc y env)])
       (smt/ite (smt/< 0 ty) (mod tx ty) (smt/- (mod (smt/- tx) ty))))]
    [_ (error 'encode "cannot encode expression ~a" v)]))

(define (enc-const v env)
  (ref! env v))

(define (enc-lit v env)
  (match v 
    [#t true]
    [#f false]
    [(? number?) ; Horrible hack to allow testing Int and Real theory before they are properly integrated. 
     (cond [(current-bitwidth) (smt/bv (finitize v) (current-bitwidth))]
           [(integer? v) (inexact->exact v)]
           [(exact? v) (smt// (numerator v) (denominator v))]
           [else v])]
    [(bv lit t) (smt/bv lit (bitvector-size t))]
    [(? enum-literal?) (ordinal v)]
    [_ (error 'enc-literal "expected a boolean?, number? or enum-literal?, given ~a" v)]))

(define-encoder rosette->smt 
  [#:== ; bool 
        [! smt/not] [&& smt/and] [|| smt/or] [=> smt/=>] [<=> smt/<=>]  
        ; num
        [ite smt/ite] [=? smt/=] [@= smt/=] [@< bvslt] [@<= bvsle] 
        [@bitwise-ior bvor] [@bitwise-and bvand] 
        [@bitwise-not bvnot] [@bitwise-xor bvxor]
        [@<< bvshl] [@>>> bvlshr] [@>> bvashr]
        [@+ bvadd] [@* bvmul] [@quotient bvsdiv] [@remainder bvsrem]
        [@abs smt/abs] [@sgn smt/sgn]
        ; bitvector
        [@bveq smt/=] [@bvslt bvslt] [@bvsle bvsle] [@bvult bvult] [@bvule bvule] 
        [@bvnot bvnot] [@bvor bvor] [@bvand bvand] [@bvxor bvxor] 
        [@bvshl bvshl] [@bvlshr bvlshr] [@bvashr bvashr]
        [@bvneg bvneg] [@bvadd bvadd] [@bvmul bvmul] [@bvudiv bvudiv] [@bvsdiv bvsdiv]
        [@bvurem bvurem] [@bvsrem bvsrem] [@bvsmod bvsmod] [@concat concat]
        ; int and real
        [$@= smt/=] [$@< smt/<] [$@<= smt/<=] 
        [$@+ smt/+] [$@* smt/*] [$@- smt/-] [$@/ smt//] 
        [$@integer->real to_real] [$@real->integer to_int] [$@int? is_int]]
  [#:?  [enum-comparison-op? smt/<]])

(define (smt/abs e)
  (smt/ite (bvslt e (smt/bv 0 (current-bitwidth))) (bvneg e) e))

(define (smt/sgn e)
  (let ([zero (smt/bv 0 (current-bitwidth))]) 
    (smt/ite (smt/= e zero) zero 
             (smt/ite (bvslt e zero) 
                      (smt/bv -1 (current-bitwidth))
                      (smt/bv  1 (current-bitwidth))))))
