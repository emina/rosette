#lang racket

(require "env.rkt" 
         (prefix-in $ "smtlib2.rkt") 
         (only-in "../../base/core/term.rkt" expression expression? constant? get-type)
         (only-in "../../base/core/polymorphic.rkt" ite ite* =? guarded-test guarded-value)
         (only-in "../../base/core/bool.rkt" @! @&& @|| @=> @<=>)
         (only-in "../../base/core/real.rkt" 
                  @integer? @real? @= @< @<= @>= @> 
                  @+ @* @- @/ @quotient @remainder @modulo 
                  @abs @integer->real @real->integer @int?)
         (only-in "../../base/core/bitvector.rkt" 
                  bitvector? bv bitvector-size 
                  @bveq @bvslt @bvsle @bvult @bvule   
                  @bvnot @bvor @bvand @bvxor @bvshl @bvlshr @bvashr
                  @bvneg @bvadd @bvmul @bvudiv @bvsdiv @bvurem @bvsrem @bvsmod
                  @concat @extract @zero-extend @sign-extend 
                  @integer->bitvector @bitvector->integer @bitvector->natural))

(provide enc)

; The enc procedure takes a value and an environment, and returns  
; an SMTLIB identifier representing that value in the given environment.  If it 
; cannot produce an encoding for the given value, an error is thrown. 
; The environment will be modified (if needed) to include an encoding for 
; the given value and all of its subexpressions (if any).
(define (enc v env)
  (ref! env v (match v
                [(? expression?) (enc-expr v env)]
                [(? constant?)   (enc-const v env)]
                [_               (enc-lit v env)])))

(define (enc-expr v env)  
  (match v
    [(and (expression (== ite*) gvs ...) (app get-type t))
     (let-values ([($0 $op) (if (bitvector? t) 
                                (values ($bv 0 (bitvector-size t)) $bvor) 
                                (values 0 $+))])
       (apply $op (for/list ([gv gvs]) 
                    ($ite (enc (guarded-test gv) env) 
                          (enc (guarded-value gv) env) 
                          $0))))]
    [(expression (== @abs) x)
     ($real-abs (enc x env) (get-type v))]
    [(expression (== @extract) i j e)
     ($extract i j (enc e env))]
    [(expression (== @sign-extend) v t)
     ($sign_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @zero-extend) v t)
     ($zero_extend (- (bitvector-size t) (bitvector-size (get-type v))) (enc v env))]
    [(expression (== @integer->bitvector) v t) 
     ($int->bv (enc v env) (bitvector-size t))]
    [(expression (== @bitvector->integer) v) 
     ($bv->int (enc v env) (bitvector-size (get-type v)))]  
    [(expression (== @bitvector->natural) v) 
     ($bv->nat (enc v env) (bitvector-size (get-type v)))]
    [(expression (app rosette->smt (? procedure? $op)) es ...) 
     (apply $op (for/list ([e es]) (enc e env)))]
    [_ (error 'enc "cannot encode ~a to SMT" v)]))

(define (enc-const v env) (ref! env v))

(define (enc-lit v env)
  (match v 
    [#t true]
    [#f false]
    [(? integer?) (inexact->exact v)]
    [(? real?) (if (exact? v) ($/ (numerator v) (denominator v)) v)]
    [(bv lit t) ($bv lit (bitvector-size t))]
    [_ (error 'enc "expected a boolean?, integer?, real?, or bitvector?, given ~a" v)]))

(define-syntax define-encoder
  (syntax-rules ()
    [(_ id [rosette-op smt-op] ...)
     (define (id op) 
       (cond [(eq? op rosette-op) smt-op] ... 
             [else #f]))]))

(define-encoder rosette->smt 
  ; core 
  [@! $not] [@&& $and] [@|| $or] [@=> $=>] [@<=> $<=>] [ite $ite] [=? $=]
  ; int and real
  [@= $=] [@< $<] [@<= $<=] 
  [@+ $+] [@* $*] [@- $-] [@/ $/]  
  [@quotient $quotient] [@remainder $remainder] [@modulo $modulo]
  [@integer->real $to_real] [@real->integer $to_int] [@int? $is_int]
  ; bitvector
  [@bveq $=] [@bvslt $bvslt] [@bvsle $bvsle] [@bvult $bvult] [@bvule $bvule] 
  [@bvnot $bvnot] [@bvor $bvor] [@bvand $bvand] [@bvxor $bvxor] 
  [@bvshl $bvshl] [@bvlshr $bvlshr] [@bvashr $bvashr]
  [@bvneg $bvneg] [@bvadd $bvadd] [@bvmul $bvmul] [@bvudiv $bvudiv] [@bvsdiv $bvsdiv]
  [@bvurem $bvurem] [@bvsrem $bvsrem] [@bvsmod $bvsmod] [@concat $concat])


(define ($quotient tx ty)
  (define tx/ty ($div ($abs tx) ($abs ty)))
  ($ite ($= ($< tx 0) ($< ty 0)) tx/ty ($- tx/ty)))

(define ($remainder tx ty)
  (define tx%ty ($mod ($abs tx) ($abs ty)))
  ($ite ($< tx 0) ($- tx%ty) tx%ty))

(define ($modulo tx ty)
  ($ite ($< 0 ty) ($mod tx ty) ($- ($mod ($- tx) ty))))

(define ($real-abs v t)
  (if (equal? t @integer?)
      ($abs v)
      ($ite ($< v 0) ($- v) v)))

(define ($int->bv i n)
  (define bv0 ($bv 0 n))
  (apply 
   $bvor 
   (let loop ([b (- n 1)] [m ($mod i (expt 2 n))])
     (if (< b 0)
         (list)
         (let* ([2^b (expt 2 b)]
                [1? ($<= 2^b m)])          
           (cons ($ite 1? ($bv 2^b n) bv0) 
                 (loop (- b 1) ($- m ($ite 1? 2^b 0)))))))))

(define ($bv->nat v n) 
  (apply $+ (for/list ([i n]) ($bit v i n))))

(define ($bv->int v n)
  (apply $+ ($- ($bit v (- n 1) n)) (for/list ([i (- n 1)]) ($bit v i n))))

(define ($bit v i n)
  (define bv0 ($bv 0 n))
  (define b (expt 2 i))
  ($ite ($= bv0 ($bvand v ($bv b n))) 0 b)) 
