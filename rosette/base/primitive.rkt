#lang racket

(require 
  (for-syntax (only-in "lift.rkt" add@ with@))
  racket/require
  racket/provide
  racket/splicing
  "safe.rkt" 
  "op.rkt"
  "type.rkt"
  "any.rkt"
  (only-in "term.rkt" expression term)
  (filtered-in add@ "bool.rkt")
  (only-in "merge.rkt" merge merge*)
  (only-in "union.rkt" union union?)
  "num.rkt" )
             
(provide 
 @boolean? @false? @number?
 (filtered-out with@ (all-defined-out))
 (filtered-out add@ 
  (combine-out 
   ! || && => <=> 
   = < <= > >=
   + - * *h / quotient remainder expt abs sgn sqrt
   << >> >>> bitwise-not bitwise-and bitwise-ior bitwise-xor 
   current-bitwidth)))

(define (impersonate-operator op origin)
  (impersonate-procedure 
   op
   (case-lambda [()          (values)]
                [(arg)       (assert-validate (op arg)  origin)]
                [(arg0 arg1) (assert-validate (op arg0 arg1) origin)]
                [args        (apply values (assert-validate (op #:rest args) origin))])))

(define-syntax (assert-validate stx)
  (syntax-case stx ()
    [(_ (op #:rest args) origin)
     #'(let ([vals (for/list ([(arg pos) (in-indexed args)]
                              [arg-type (apply op/arg-types op args)])
                     (assert-type arg-type arg op origin))])
         (assert-precondition (apply (op-pre op) vals) op origin)
         vals)]
    [(_ (op arg) origin)
     #'(let ([val (assert-type (first (op/arg-types op arg)) arg op origin)])
         (assert-precondition ((op-pre op) val) op origin)
         val)]
    [(_ (op arg0 arg1) origin)
     #'(let* ([arg-types (op/arg-types op arg0 arg1)]
              [val0 (assert-type (first arg-types) arg0 op origin)]
              [val1 (assert-type (second arg-types) arg1 op origin)])
         (assert-precondition ((op-pre op) val0 val1) op origin)
         (values val0 val1))]))

(define (assert-type arg-type arg-val op origin)
  (let*-values ([(can-cast? val) (cast arg-type arg-val)])
    (assert can-cast?
            (type-error (op-name op) arg-type arg-val)                      
            origin)
    val))

(define (assert-precondition pre op origin)
  (assert pre (thunk (error (op-name op) "precondition violation")) origin))

(define-syntax-rule (define-impersonator id op)
  (define id (impersonate-operator op #'id)))

(define-syntax-rule (define-impersonators [id op] ...)
  (begin (define-impersonator id op) ...))

(define-impersonators 
  [! @!] [&& @&&] [|| @||] [=> @=>] [<=> @<=>] 
  [= @=] [> @>] [>= @>=] [< @<] [<= @<=]
  [+ @+] [- @-] [* @*] [/ @/]
  [quotient @quotient] [remainder @remainder] [expt @expt] [*h @*h]
  [abs @abs] [sgn @sgn] [sqrt @sqrt] 
  [<< @<<] [>> @>>] [>>> @>>>]
  [bitwise-not @bitwise-not] [bitwise-and @bitwise-and] 
  [bitwise-ior @bitwise-ior] [bitwise-xor @bitwise-xor])

(define @boolean=? <=>)
(define (@add1 x) (+ x 1))
(define (@sub1 x) (- x 1))

(define (extreme op)
  (case-lambda [(x) x]
               [(x y) (merge (op x y) x y)]
               [(x . ys) (let loop ([e x] [ys ys])
                           (if (null? ys)
                               e
                               (let ([c (car ys)])
                               (loop (merge (op e c) e c) (cdr ys)))))]))
(define @max (extreme >))
(define @min (extreme <))

(define-syntax-rule (define-real-operator op racket/op)
  (define (op x)
    (match x
      [(? number?) (racket/op x)]
      [(term _ (== @number?)) x]
      [(? union?) (truncate (coerce x @number?))]
      [_ (racket/op x)])))

(define-real-operator @truncate truncate)
(define-real-operator @inexact->exact inexact->exact)
(define-real-operator @exact->inexact exact->inexact)
(define-real-operator @round round)

(define (@integer? x)
  (match x 
    [(? integer?) #t]
    [(term _ (== @number?)) #t]
    [(union vs) (apply merge* (for/list ([gv vs]) (cons (car gv) (@integer? (cdr gv)))))]
    [_ #f]))
                    
(define (@positive? x) (> x 0))
(define (@negative? x) (< x 0))
(define (@zero? x) (= x 0))
(define (@even? x) (if (number? x) (even? x) (= 0 (bitwise-and x 1))))
(define (@odd? x)  (if (number? x) (odd? x)  (= 1 (bitwise-and x 1))))

(define (@bitwise-bit-set? n m)
  (if (and (number? n) (number? m))
      (bitwise-bit-set? n m)
      (! (= 0 (bitwise-and n (<< 1 m))))))

(define (@bitwise-bit-field n start end)
  (if (and (number? n) (number? start) (number? end))
      (bitwise-bit-field n start end)
      (begin
        (assert (>= start 0))
        (assert (>= end start))
        (bitwise-and (sub1 (<< 1 (- end start))) (>> n start)))))