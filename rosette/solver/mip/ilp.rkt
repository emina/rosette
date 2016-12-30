#lang racket

(require "common.rkt"
         (only-in "../../base/core/term.rkt" expression expression? constant? term? get-type @app type-of)
         (only-in "../../base/core/polymorphic.rkt" ite ite* =? guarded-test guarded-value)
         (only-in "../../base/core/distinct.rkt" @distinct?)
         (only-in "../../base/core/bool.rkt" @! @&& @|| @=> @<=> @forall @exists)
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

(provide ilp-start ilp-done
         ilp-minimize ilp-maximize
         ilp-assert-init ilp-enc
         rosette->ilp)

(struct equation (op signs terms lit))

(define vars (set))

(define-syntax-rule (ilp-printf arg ...)
  ;(fprintf (current-error-port) arg ...)
  ;(fprintf out-port arg ...)
  (fprintf (current-output-port) arg ...)
  )

(define (ilp-start)
  (ilp-printf "enter example\n")
  (set! vars (set)))

(define (ilp-done)
  ;; TODO: set bounds
  (define integer-vars (filter (lambda (x) (eq? (type-of x) @integer?)) (set->list vars)))
  (unless (set-empty? integer-vars)
    (define count 0)
    (ilp-printf "generals\n")
    (for ([v integer-vars])
      (set! count (add1 count))
      (when (= count 200)
        (ilp-printf "\n")
        (set! count 0))
      (ilp-printf "~a " (get-name v)))
    (ilp-printf "\n")
    )
  (ilp-printf "end\n")
  (ilp-printf "optimize\n")
  (ilp-printf "display solution variables -\n")
  )

(define (ilp-minimize x env)
  (ilp-printf "minimize ~a\n" (get-name x)))

(define (ilp-maximize x env)
  (ilp-printf "maximize ~a\n" (get-name x)))

(define (ilp-assert-init)
  (ilp-printf "subject to\n"))

(define (ilp-enc v env)
  ;(fprintf (current-error-port) "enc ~a\n" v)
  (rosette->ilp v env))

(define (get-name v)
  (define name (format "~a" v))
  (string-replace name "$" ""))

(define (print-equation v)
  (match v
    [(equation op signs terms lit)
     (for ([sign signs]
           [term terms])
       (ilp-printf "~a " (if (> sign 0) '+ '-))
       (print-expr term))
     (ilp-printf "~a ~a\n" (op->string op) lit)]))

(define (print-expr v)
  (match v
    [(expression (== @*) e1 e2)
     (print-expr e1)
     (print-expr e2)]
    [(? constant?)
     (ilp-printf "~a " (get-name v)) (set! vars (set-add vars v))]
    [(? number?)    (ilp-printf "~a " v) ]
    [_              (raise (exn:fail (format "cannot encode term ~a" v) (current-continuation-marks)))]))

(define-syntax define-encoder
  (syntax-rules ()
    [(_ id [rosette-op smt-op] ...)
     (define (id op) 
       (cond [(eq? op rosette-op) smt-op] ... 
             [else (raise (exn:fail (format "unknown op ~a" op) (current-continuation-marks)))]))]))

(define-encoder op->string
  [@= '=] [@<= '<=] [@< '<] [@+ '+] [@* '||] [@- '-])

; Convert rosette constraint v to an ILP equation
(define (rosette->ilp v env)
  ;(fprintf (current-error-port) "~a\n" v)
  (match v
    [(expression (== @&&) es ...)
     (for ([e es]) (rosette->ilp e env))
     ]
    [(expression op e1 e2)
     (unless (member op (list @= @<= @<))
       (raise (exn:fail (format "Cannot encode inequality operation ~a to ILP in ~a" op v)
                         (current-continuation-marks))))
     
     (define-values (e1-signs e1-terms e1-lits e1-all-int) (flatten-expr e1 env))
     (define-values (e2-signs e2-terms e2-lits e2-all-int) (flatten-expr e2 env))
     ;(ilp-printf "~a\n" `(LHS ,e1-signs ,e1-terms ,e1-lits))
     ;(ilp-printf "~a\n" `(RHS ,e2-signs ,e2-terms ,e2-lits))
     (define signs (append e1-signs (map - e2-signs)))
     (define terms (append e1-terms e2-terms))
     (define lits (append (map - e1-lits) e2-lits))
     (define lit (apply + lits))

     (cond
       [(and (equal? op @<) e1-all-int e2-all-int)
        (print-equation (equation @<= signs terms (sub1 lit)))]
       [else
        (print-equation (equation op signs terms lit))])
     ]
    [_
     (raise (exn:fail (format "Cannot encode ~a to ILP" v)
                       (current-continuation-marks)))]))

(define (flatten-expr v env)
  (define all-int #t)
  (define signs (list))
  (define terms (list))
  (define lits (list))
  (define (f v sign)
    (match v
      [(expression (== @*) e1 e2)
       (set! v (distribute v))
       (match v
         [(expression (== @*) e1 e2)
          (set! signs (cons sign signs))
          (set! terms (cons v terms))]
         [_ (f v sign)])
       ]
      [(expression (== @+) es ...)
       (for ([e es]) (f e sign))]
      [(expression (== @-) e)
       (f e (- sign))]
      [(expression (== @-) es ...)
       (f (car es) sign)
       (for ([e (cdr es)]) (f e (- sign)))]
      ;[(expression (== @integer->real) e1)  (f e1 sign)]
      ;[(expression (== @real->integer) e1)  (f e1 sign)]
      [(? constant?)   (set! signs (cons sign signs)) (set! terms (cons v terms))
                       (unless (eq? (type-of v) @integer?) (set! all-int #f))]
      [(? integer?)    (set! lits (cons (* sign v) lits))]
      [(? real?)       (set! lits (cons (* sign v) lits)) (set! all-int #f)]
      [_               (raise (exn:fail (format "cannot encode ~a to ILP" v)
                                         (current-continuation-marks)))]
    ))
  (f v 1)
  (values signs terms lits all-int))

(define (distribute v)
  (define (f e1 e2) ;; e1 is number
    (match e2
      [(expression (== @*) es2 ...)
       (define num (findf number? es2))
       (unless num (exn:fail (format "cannot encode ~a to ILP" v)) (current-continuation-marks))
       (distribute (apply expression @* (* num e1) (remove num es2)))
       ]

      [(expression op2 es2 ...)
       (unless (member op2 (list @+ @-))
         (raise (exn:fail (format "cannot encode ~a to ILP" v)) (current-continuation-marks)))
       (apply expression op2
              (for/list ([e es2])
                (if (number? e)
                    (* e1 e)
                    (distribute (expression @* e1 e)))))]))
              
  (match v
    [(expression op e1 e2)
     (cond
       [(and (number? e1) (number? e2)) (* e1 e2)]
       [(and (number? e1) (constant? e2)) v]
       [(and (number? e2) (constant? e1)) (expression @* e2 e1)]
       [(number? e1) (f e1 e2)]
       [(number? e2) (f e2 e1)]
       [else (raise (exn:fail (format "distribute: cannot encode ~a to ILP" v)
                               (current-continuation-marks)))])]))
        
        