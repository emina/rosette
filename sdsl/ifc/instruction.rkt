#lang rosette

(require rosette/lib/match rosette/lib/angelic "value.rkt")

(provide program instruction
         (rename-out [inst? instruction?]
                     [inst-proc procedure]
                     [inst-args args]))

; An instruction holds a procedure (-> machine? value? ..k), and a 
; list of k values. Applying an instruction i to a machine m evaluates
; the expression (apply (inst-proc i) m (inst-args i)).
(struct inst (proc args) 
  #:transparent
  #:reflection-name 'instruction
  #:property prop:procedure
  (lambda (self m)
    (match (inst-proc self) ; Use match in case inst-proc is symbolic.
      [(? procedure? proc) (apply proc m (inst-args self))]))
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~s" (cons (object-name (inst-proc self)) (inst-args self))))])

; Returns a fresh instruction that wraps the given proc 
; and the given arguments, if given.  If no arguments are 
; given, and the procedure accepts k arguments in addition 
; to the first machine? argument, the instruction is 
; initialized with a list of k fresh symbolic values.
; (-> procedure? [#:rest value?] inst?)
(define make-inst
  (case-lambda 
    [(proc)
     ; Use match in case proc is a symbolic reference---that is, a 
     ; symbolic representation of a procedure.  The use of this form 
     ; ensures that the instruction is correctly constructed for each 
     ; concrete procedure to which the proc reference may be resolved 
     ; by the solver.  This is needed since operations (such as apply 
     ; or procedure-arity) on Racket's procedure data type are not yet 
     ; lifted by rosette itself.
     (match proc 
       [(? procedure? proc)
        (inst proc (for/list ([i (sub1 (procedure-arity proc))]) 
                     (@ (?int) (?label))))])]
    [(proc . args)
     (inst proc args)]))

; We'll export make-inst as instruction constructor and inst as 
; deconstructor for pattern matching.
(define-match-expander instruction
  (syntax-rules () 
    [(_ proc args) (inst proc args)])
  (syntax-id-rules ()
    [(instruction proc arg ...) (make-inst proc arg ...)]
    [instruction                make-inst]))

; Returns thunk that prodcues a list of fresh symbolic instructions. If given a list of k 
; procedures, the thunk's output list contains k instructions such that the ith 
; instruction wraps the ith procedure.  If given an integer LOC and a list
; of procedures procs, the thunk's output list contains LOC instructions such 
; that the ith instruction wraps any of the procedures in procs. 
(define program
  (case-lambda 
    [(procs)     (thunk (map instruction procs))]
    [(LOC procs) (thunk (for/list ([i LOC])
                          (instruction (apply choose* procs))))])) 
 



