#lang racket

(require "common.rkt"
         (only-in "../../base/core/term.rkt" expression expression? constant? term? get-type @app type-of)
         (only-in "../../base/core/bool.rkt" @! @&& @|| @=> @<=> @forall @exists)
         (only-in "../../base/core/real.rkt" 
                  @integer? @real? @= @< @<= @>= @> 
                  @+ @* @- @/ @quotient @remainder @modulo 
                  @abs @integer->real @real->integer @int?))

(provide mip-start mip-done
         mip-minimize mip-maximize
         mip-assert-start mip-enc)

(define vars (set))

(define-syntax-rule (mip-printf arg ...)
  (fprintf (current-output-port) arg ...))

; Print CPLEX command to enter MIP constraints.
(define (mip-start)
  (mip-printf "enter example\n")
  (set! vars (set)))

; After entering constaints
;  - declare integer variables.
;  - print comment to solve the query.
;  - print solution.
(define (mip-done)
  (define integer-vars (filter (lambda (x) (eq? (type-of x) @integer?)) (set->list vars)))
  (unless (set-empty? integer-vars)
    (define count 0)
    (mip-printf "generals\n")
    (for ([v integer-vars])
      (set! count (add1 count))
      (when (= count 200)
        ;; Print newline after every 200 variables because CPLEX limits how long each line can be.
        (mip-printf "\n")
        (set! count 0))
      (mip-printf "~a " (get-name v)))
    (mip-printf "\n")
    )
  (mip-printf "end\n")
  )

; Print a minization objective.
(define (mip-minimize x)
  (mip-printf "minimize ")
  (flatten-and-print-expr x)
  (mip-printf "\n"))

; Print a maximization objective.
(define (mip-maximize x)
  (mip-printf "maximize ")
  (flatten-and-print-expr x)
  (mip-printf "\n"))

; Print an indicator for the beginning of hard constraints.
(define (mip-assert-start)
  (mip-printf "subject to\n"))

; Print a constraint.
(define (mip-enc v)
  (rosette->mip v))

(struct equation (op signs terms lit))

; Flatten an expression v and print it out in CPLEX format.
(define (flatten-and-print-expr v)
  (define-values (signs terms lits all-int) (flatten-expr v))
  (for ([sign signs]
        [term terms])
    (mip-printf "~a " (if (> sign 0) '+ '-))
    (print-term term))
  (for ([lit lits])
    (mip-printf "~a " (if (>= lit 0) '+ '-))
    (print-term lit)))

; Given a equation object, print it out in CPLEX format.
(define (print-equation v)
  (match v
    [(equation op signs terms lit)
     (for ([sign signs]
           [term terms])
       (mip-printf "~a " (if (> sign 0) '+ '-))
       (print-term term))
     (mip-printf "~a ~a\n" (op->string op) lit)]))

; Given a term, print it out in CPLEX format.
(define (print-term v)
  (match v
    [(expression (== @*) e1 e2)
     (print-term e1)
     (print-term e2)]
    [(? constant?)
     (mip-printf "~a " (get-name v)) (set! vars (set-add vars v))]
    [(? number?)
     (if (integer? v)
         (mip-printf "~a " v)
         (mip-printf "~a " (exact->inexact v)))]
    [_              (raise (exn:fail (format "Cannot encode term ~a" v) (current-continuation-marks)))]))

(define-syntax define-encoder
  (syntax-rules ()
    [(_ id [rosette-op smt-op] ...)
     (define (id op) 
       (cond [(eq? op rosette-op) smt-op] ... 
             [else (raise (exn:fail (format "unknown op ~a" op) (current-continuation-marks)))]))]))

(define-encoder op->string
  [@= '=] [@<= '<=] [@< '<] [@+ '+] [@* '||] [@- '-])

; Convert rosette constraint v to an equation object, and print it.
(define (rosette->mip v)
  (match v
    [(expression (== @&&) es ...)
     (for ([e es]) (rosette->mip e))
     ]
    [(expression op e1 e2)
     (unless (member op (list @= @<= @<))
       (raise (exn:fail (format "Cannot encode inequality operation ~a to MIP in ~a" op v)
                         (current-continuation-marks))))
     
     (define-values (e1-signs e1-terms e1-lits e1-all-int) (flatten-expr e1))
     (define-values (e2-signs e2-terms e2-lits e2-all-int) (flatten-expr e2))
     (define signs (append e1-signs (map - e2-signs)))
     (define terms (append e1-terms e2-terms))
     
     ;; Collapse terms that use the same variables.
     ;; However, it doesn't make CPLEX solve any faster.
     ;; Sometimes it reduces time to print to file, but also increases overhead.
     ;(define-values (terms signs) (simplify-equation pre-terms pre-signs))
     
     (define lits (append (map - e1-lits) e2-lits))
     (define lit (apply + lits))

     (cond
       [(and (equal? op @<) e1-all-int e2-all-int)
        (print-equation (equation @<= signs terms (sub1 lit)))]
       [else
        (print-equation (equation op signs terms lit))])
     ]
    [_
     (raise (exn:fail (format "Cannot encode ~a to mip (1)" v)
                       (current-continuation-marks)))]))

; Flattern a Rosette expression v to
;  - terms:   a list of (* c term)
;  - signs:   a corresponding ist of sign (1 or -1)
;  - lits:    a list of literals
;  - all-int: an indicator informing if all numbers and terms in v are integers.
(define (flatten-expr v)
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
      [(expression (== @integer->real) e1)  (f e1 sign)]
      [(expression (== @real->integer) e1)  (f e1 sign)]
      [(? constant?)   (set! signs (cons sign signs)) (set! terms (cons v terms))
                       (unless (eq? (type-of v) @integer?) (set! all-int #f))]
      [(? integer?)    (set! lits (cons (* sign v) lits))]
      [(? real?)       (set! lits (cons (* sign v) lits)) (set! all-int #f)]
      [_               (raise (exn:fail (format "Cannot encode ~a to MIP (2)" v)
                                         (current-continuation-marks)))]
    ))
  (f v 1)
  (values signs terms lits all-int))

; Given an expression v of the form (* e1 e2),
; distribute e1 into e2 or vice versa.
(define (distribute v)
  (define (f e1 e2) ;; e1 is number
    (match e2
      [(expression (== @*) es2 ...)
       (define num (findf number? es2))
       (unless num (exn:fail (format "Cannot encode ~a to MIP (3)" v) (current-continuation-marks)))
       (distribute (apply expression @* (* num e1) (remove num es2)))
       ]

      [(expression (== @integer->real) e2-inner)
       (distribute (expression @* e1 e2-inner))
       ]
      
      [(expression op2 es2 ...)
       (unless (member op2 (list @+ @-))
         (raise (exn:fail (format "Cannot encode ~a to MIP (4)" v) (current-continuation-marks))))
       (apply expression op2
              (for/list ([e es2])
                (if (number? e)
                    (* e1 e)
                    (distribute (expression @* e1 e)))))]

      ))
              
  (match v
    [(expression op e1 e2)
     (cond
       [(and (number? e1) (number? e2)) (* e1 e2)]
       [(and (number? e1) (constant? e2)) v]
       [(and (number? e2) (constant? e1)) (expression @* e2 e1)]
       [(number? e1) (f e1 e2)]
       [(number? e2) (f e2 e1)]
       [else (raise (exn:fail (format "distribute: cannot encode ~a to MIP" v)
                               (current-continuation-marks)))])]))
        
        