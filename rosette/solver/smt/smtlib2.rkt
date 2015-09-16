#lang racket

(require racket/syntax (only-in racket [< racket/<] [- racket/-]))

(provide (except-out (all-defined-out) define-ops smt-port print-cmd))

; All command functions from this module (e.g., set-logic)
; write their SMT command to smt-port.
(define smt-port 
  (make-parameter (current-output-port)
                  (lambda (port)
                    (unless (output-port? port)
                      (error 'smt-port "expected an output-port?, given ~a" port))
                    port)))

; Reads the SMT solution from the given input port.
; The solution consist of 'sat or 'unsat, followed by  
; followed by a suitably formatted s-expression.  The 
; output of this procedure is a hashtable from constant 
; identifiers to their values (if the solution is 'sat);
; a non-empty list of assertion identifiers that form an
; unsatisfiable core (if the solution is 'unsat and a 
; core was extracted); or #f (if the solution is 
; 'unsat and no core was extracted).
(define (read-solution port)
  (match (read port)
    [(== 'sat) 
     (match (read port)
       [(list (== 'model) (list (== 'define-fun) const _ _ val) ...)
        (for/hash ([c const] [v val]) (values c v))]
       [other (error 'solution "expected model, given ~a" other)])]
    [(== 'unsat) (read port) #f] ; TODO: deal with cores
    [other (error 'smt-solution "unrecognized solver output: ~a" other)]))

; Reads the response to get-info command from the given input port.
(define (read-info port)
  (match (read port)
    [(list _ info) info]
    [other (error 'read-info "expected info, given ~a" other)]))

(define-syntax-rule (print-cmd arg ...)
  (begin 
    ;(printf  arg ...)
    (fprintf (smt-port) arg ...)))

; Prints all SMT commands issued during the dynamic
; extent of the given expressions to the provided port.
(define-syntax-rule (cmd [port] expr ...)
  (parameterize ([smt-port port])
    expr ...
    (flush-output port)))

; Commands
(define (set-logic l) (print-cmd "(set-logic ~a)" l))
(define (check-sat)   (print-cmd "(check-sat)\n"))
(define (get-model)   (print-cmd "(get-model)\n"))
(define (get-info kw) (print-cmd "(get-info ~a)\n" kw))

(define (reset)       (print-cmd "(reset)\n"))

(define (push n)      (print-cmd "(push ~a)\n" n))
(define (pop n)       (print-cmd "(pop ~a)\n" n))
(define (assert expr) (print-cmd "(assert ~a)" expr))

; Declarations and definitions
(define (declare-const id type)
  (print-cmd "(declare-const ~a ~a)" id type))

(define (define-const id type body)
  (print-cmd "(define-fun ~a () ~a ~a)" id type body))

(define-syntax-rule (define-ops id ...)
  (define-values (id ...)
    (values (lambda e `(id ,@e)) ...)))

; Core theory
(define Bool 'Bool)
(define true 'true)
(define false 'false)
(define-ops not and or xor => ite =)
(define (<=> l r) (and (=> l r) (=> r l)))

; Bitvector theory
(define (BitVec size) `(_ BitVec ,size))
(define (bv val size)  (if (racket/< val 0)
                           (bvneg `(_ ,(format-symbol "bv~a" (racket/- val)) ,size))
                           `(_ ,(format-symbol "bv~a" val) ,size)))
(define-ops 
  bvnot bvand bvor bvxor 
  bvule bvult bvuge bvugt bvsle bvslt bvsge bvsgt
  bvneg bvadd bvsub bvmul bvsdiv bvudiv bvurem bvsrem bvsmod
  bvshl bvlshr bvashr concat) 

(define (extract i j s)
  `((_ extract ,i ,j) ,s))

; Int theory
(define Int 'Int)
(define-ops < <=)
