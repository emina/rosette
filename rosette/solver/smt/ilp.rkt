#lang racket

(require (only-in "../../base/core/term.rkt" expression expression? constant? term? get-type @app type-of)
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

(provide ilp-reset ilp-done
         ilp-minimize ilp-maximize
         ilp-assert-init ilp-enc)

(struct equation (op signs terms lit))

(define path "/Users/mangpo/work/scheduler/tests/out.mip")
(define vars (set))
(define out-port #f)

(define-syntax-rule (ilp-printf arg ...)
  ;(fprintf (current-error-port) arg ...)
  (fprintf out-port arg ...))

(define (ilp-reset)
  (fprintf (current-error-port) "Generate ILP program at ~a\n" path)
  (set! out-port (open-output-file path #:exists 'truncate))
  (ilp-printf "enter example\n")
  (set! vars (set)))

(define (ilp-done)
  ;; TODO: set bounds
  (define integer-vars (filter (lambda (x) (eq? (type-of x) @integer?)) (set->list vars)))
  (unless (set-empty? integer-vars)
    (ilp-printf "generals\n")
    (for ([v integer-vars])
      (ilp-printf "~a " (get-name v)))
    (ilp-printf "\n")
    )
  (ilp-printf "end\n")
  (ilp-printf "optimize\n")
  (ilp-printf "display solution variables LAT\n")
  (close-output-port out-port))

(define (ilp-minimize x env)
  (ilp-printf "minimize ~a\n" x))

(define (ilp-maximize x env)
  (ilp-printf "maximize ~a\n" x))

(define (ilp-assert-init)
  (ilp-printf "subject to\n"))

(define (ilp-enc v env)
  ;(ilp-printf "enc ~a\n" v)
  ;(ilp-printf "env ~a\n" env)
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
    [_              (raise (format "cannot encode term ~a" v))]))

(define-syntax define-encoder
  (syntax-rules ()
    [(_ id [rosette-op smt-op] ...)
     (define (id op) 
       (cond [(eq? op rosette-op) smt-op] ... 
             [else (raise (format "unknown op ~a" op))]))]))

(define-encoder op->string
  [@= '=] [@<= '<=] [@+ '+] [@* '||] [@- '-])

; Convert rosette constraint v to an ILP equation
(define (rosette->ilp v env)
  (match v
    [(expression (== @&&) es ...)
     (for ([e es]) (rosette->ilp e env))
     ]
    [(expression op e1 e2)
     (unless (member op (list @= @<=))
       (raise (format "cannot encode inequality operation ~a to ILP" op)))
     
     (define-values (e1-signs e1-terms e1-lits) (flatten-expr e1 env))
     (define-values (e2-signs e2-terms e2-lits) (flatten-expr e2 env))
     ;(ilp-printf "~a\n" `(LHS ,e1-signs ,e1-terms ,e1-lits))
     ;(ilp-printf "~a\n" `(RHS ,e2-signs ,e2-terms ,e2-lits))
     (define signs (append e1-signs (map - e2-signs)))
     (define terms (append e1-terms e2-terms))
     (define lits (append (map - e1-lits) e2-lits))
     (print-equation (equation op signs terms (apply + lits)))
     ]
    [_ (raise (format "cannot encode ~a to ILP" v))]))

(define (flatten-expr v env)
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
      [(expression (== @-) es ...)
       (f (car es) sign)
       (for ([e (cdr es)]) (f e (- sign)))]
      [(expression (== @integer->real) e1)  (f e1 sign)]
      [(expression (== @real->integer) e1)  (f e1 sign)]
      [(? constant?)   (set! signs (cons sign signs)) (set! terms (cons v terms))]
      [(? number?)     (set! lits (cons (* sign v) lits))]
      [_               (raise (format "cannot encode ~a to ILP" v))]
    ))
  (f v 1)
  (values signs terms lits))

(define (distribute v)
  (define (f e1 e2)
    (match e2
      [(expression op2 es2 ...)
       (unless (member op2 (list @+ @-))
         (raise (format "cannot encode ~a to ILP" v)))
       (apply expression op2
              (for/list ([e es2])
                (expression @* e1 e)))]))
              
  (match v
    [(expression op e1 e2)
     (cond
       [(and (number? e1) (number? e2)) (* e1 e2)]
       [(and (number? e1) (constant? e2)) v]
       [(and (number? e2) (constant? e1)) (expression @* e2 e1)]
       [(number? e1) (f e1 e2)]
       [(number? e2) (f e2 e1)]
       [else (raise (format "distribute: cannot encode ~a to ILP" v))])]))
        
        