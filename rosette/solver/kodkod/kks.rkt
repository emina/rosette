#lang racket

(require racket/syntax)

(provide (except-out (all-defined-out) kodkod-port print-cmd print-eof define-ops))

; Prints all Kodkod commands issued during the dynamic
; extent of the given expressions to the provided port.
(define-syntax-rule (cmd [port] expr ...)
  (parameterize ([kodkod-port port])
    expr ...
    (flush-output port)))


; All command functions from this module (e.g., solve)
; write their Kodkod command to kodkod-port.
(define kodkod-port 
  (make-parameter (current-output-port)
                  (lambda (port)
                    (unless (output-port? port)
                      (error 'kodkod-port "expected an output-port?, given ~a" port))
                    port)))

; Prints the given command string to the kodkod-port.
(define-syntax-rule (print-cmd arg ...)
  (begin 
    ;(printf  arg ...) (newline)
    (fprintf (kodkod-port) arg ...)))

(define (print-eof) 
  (display #\uFFFF (kodkod-port)))

; Commands
(define (configure . kvs) (print-cmd "(configure ~a)" (keyword-apply ~a '(#:separator) '(" ") kvs)))
(define (assert val)      (print-cmd "(assert ~a)" val))
(define (solve)           (print-cmd "(solve)") (print-eof))  
(define (clear)           (print-cmd "(clear)") (print-eof))

;; Declarations and definitions
(define (define-const id val) (print-cmd "(~a ~a)" id val))
(define (declare-univ size)   (print-cmd "(univ ~a)" size))

(define (declare-ints ints idxs)
  (print-cmd "(ints [")
  (for ([int ints][idx idxs])
    (print-cmd "(~a ~a)" int idx))
  (print-cmd "])"))

(define declare-const
  (case-lambda 
    [(id lo hi) (print-cmd "(~a [~a ~a])" id lo hi)]
    [(id exact) (print-cmd "(~a [~a])" id exact)]))

; Identifiers
(define (r idx) (format-symbol "r~a" idx))  ; relational constant
(define (e idx) (format-symbol "e~a" idx))  ; relational expression
(define (f idx) (format-symbol "f~a" idx))  ; boolean expression
(define (i idx) (format-symbol "i~a" idx))  ; bitvector expression

; Built-in constants
(define-values (TRUE FALSE UNIV NONE IDEN INTS) 
  (apply values '(true false univ none iden ints)))

; Tuples
(define tuple list)
(define-syntax tupleset
  (syntax-rules ()
    [(_ #:range from to) (format "{~a ... ~a}" from to)]
    [(_ #:area  from to) (format "{~a # ~a}" from to)]
    [(_ #:tuples ts)     (format "{~a}" (keyword-apply ~a '(#:separator) '(" ") ts))]
    [(_ t ...)           (format "{~a}" (~a t ... #:separator " "))]))
    
; Operators
(define-syntax-rule (define-ops [id symbol] ...)
  (define-values (id ...)
    (values (lambda e `(symbol ,@e)) ...)))

(define-ops 
  ; polymorphic
  [ite ite] [= =]                                     
  
  ; boolean
  [not !] [and &&] [or \|\|] [<=> <=>] [=> =>]   
  
  ; bitvector
  [set set] [bvabs abs] [bvsgn sgn] [bvneg -] [bvnot ~]          
  [bvslt <] [bvsle <=] [bvand &] [bvor \|] [bvxor ^] 
  [bvshl <<] [bvlshr >>>] [bvashr >>]
  [bvadd +] [bvsub -] [bvmul *] [bvsdiv /] [bvsrem %]
  
  ; relational
  [no no] [lone lone] [one one] [some some] [sum sum] [-> ->] [in in])

; Reads a solution to a Kodkod problem from specified 
; input port and returns a hashtable or a list.
;
; If the problem  is satisfiable, the result is a 
; hashtable mapping relation identifiers to sets of 
; tuples.  A set of tuples is represented as a list of 
; lists (of natural numbers), with no duplicates.  
;
; If the problem is unsatisfiable, the result is a set 
; of formula identifiers, represented as a list with no 
; duplicates, which identify the formulas in the problem 
; that form a minimal unsatisfiable core.  The produced 
; list will be non-empty if the underlying solver was 
; instructed to provide cores; it will be empty otherwise.
(define (read-solution port)
  (match (read port)
    [(list (== 'sat) (== ':model) (list (list rid val) ...))
     (for/hash ([r rid][tuples val]) (values r tuples))]
    [(list (== 'unsat) (== ':core) (list fid ...)) fid]
    [(list (== 'unsat)) '()]
    [other (error 'read-solution "Unrecognized solver output: ~a" other)]))



  


