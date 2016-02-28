#lang rosette

(require (only-in racket [eq? ==]) 
         (only-in "bvops.rkt" bv bv*))

(provide (all-defined-out))

; Represents an operator in the BV language, 
; consisting of an id and a procedure.
(struct op (id proc)
  #:transparent
  #:property prop:procedure 
  (struct-field-index proc)
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (syntax->datum (op-id self))))])

; Returns the arity of the given operator.
(define (op-arity op)
  (match (procedure-arity op)
    [(? integer? a) a]
    [_ 2]))

; Represents an instruction in a BV program, 
; consisting of an op, the integer identifer of the 
; output register, and a list of integer identifiers 
; of the input registers.
(struct inst (op out in) 
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" 
              `(:= ,(inst-out self)
                   (,(inst-op self) ,@(inst-in self)))))])

; Represents a BV program, which specifies the number of 
; argument registers and an unordered list of instructions.
; The output of a program is computed by the instruction with 
; the out register (prog-out p).
; See the well-formed-program procedure for constraints on well-formed programs.
(struct prog (args insts) 
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(prog ~a ~a)" (prog-args self) (prog-insts self)))])

; Represents an execution trace for the given BV program.  
; The trace keeps track of the argument values, as well as values 
; stored and loaded by each instruction.
; In particular, args is a list of (prog-args (trace-prog t)) 
; bitvecotr values, while access is a list of lists of bitvectors.  
; The ith access list contains the value stored and the 
; values loaded by the ith instruction in (prog-insts (trace-prog t)).
; See the well-formed-trace procedure for constraints on well-formed traces.
(struct trace (prog args access) 
  #:transparent)

; Constructs a symbolic instruction for the given op.
(define (inst* op)
  (define (reg*)
    (define-symbolic* r integer?)
    r)
  (inst op (reg*) (for/list ([i (op-arity op)]) (reg*))))

; Constructs a symbolic program with the given number of arguments 
; and (length ops) instructions, where the ith instruction wraps the ith op.
(define (prog* args ops)  
  (prog args (map inst* ops)))

; Constructs a symbolic trace for the given program and, optionally, inputs.
(define (trace* prog [args #f])
  (trace prog
         (or args (for/list ([arg (prog-args prog)]) (bv*)))
         (for/list ([inst (prog-insts prog)])
           (let ([reads (for/list ([in (inst-in inst)]) (bv*))])
             (cons (apply (inst-op inst) reads) reads)))))

; Returns the output register for the given program.
(define (prog-out p)
  (match-define (prog I (app length M)) p)
  (sub1 (+ I M)))

; Takes as input a BV program, and checks that its instructions form a 
; an SSA program when sorted according to their output register.  
; As in Gulwani et al. "Synthesis of Loop-Free Programs", 
; PLDI'11, registers are ordered, and we identify the ith input with 
; the register i.  The output registers of the instructions in the 
; given library must therefore be unique integers drawn from (prog-args p) to 
; (prog-out p), inlusive.  We further require 
; that the input registers obey the SSA constraint:  an instruction's input register must 
; be smaller than its output register.  This procedure returns void iff the 
; program is well-formed; otherwise it fails with an assertion violation.
(define (well-formed-program p)
  (match-define (prog I insts) p)
  (define M (prog-out p))
  
  ; Consistency: Every instruction has a unique output location.
  (do ([xs insts (cdr xs)]) ((null? xs))
    (let ([x0 (car xs)])
      (for ([x (cdr xs)])
        (assert (not (= (inst-out x0) (inst-out x)))))))
  
  ; Acyclicity: The location of every instruction's input precedes the location of its output.
  ; Location validity:  All input locations are between 0 and M, exclusive, and all
  ; output locations are between I and M, inclusive.
  (for ([x insts])
    (let ([x-out (inst-out x)])
      (for ([x-in (inst-in x)])
        (assert (< x-in x-out))            ; acyclicity
        (assert (<= 0 x-in M)))            ; input validity 
      (assert (<= I x-out M)))))           ; output validity


; Takes as input a trace of a BV program, and checks that  
; the trace is consistent with the program semantics.  As in Gulwani et al. "Synthesis of Loop-Free Programs", 
; PLDI'11, locations are ordered, and we identify the ith input with 
; the location i.  We require the location 
; assignment to be consistent with instruction semantics:  applying 
; an instruction to the values drawn from the specified locations yields 
; the instruction's output value.  This procedure terminates 
; succesfully iff the given library of instructions is consistent over
; the given inputs; otherwise it fails with an assertion violation.
(define (well-formed-trace t)
  (match-define (trace (prog _ insts) args access) t)
  
  (for ([(arg-val arg-reg) (in-indexed args)])
    (for ([x insts] [x-vals access])
      (for ([x-reg (inst-in x)] 
            [x-val (cdr x-vals)])
        (assert (=> (equal? x-reg arg-reg) (equal? x-val arg-val))))))
  
  (for ([x insts] [x-vals access])
    (let ([x-reg (inst-out x)] 
          [x-val (car x-vals)])
      (for ([y insts] 
            [y-vals access]
            #:unless (== x y))
        (for ([y-reg (inst-in y)]
              [y-val (cdr y-vals)])
          (assert (=> (equal? x-reg y-reg) (equal? x-val y-val))))))))


; Checks that the given trace produces the specifies output. 
(define (trace-out t expected)
  (match-define (trace (and p (prog _ insts)) _ access) t)
  (define M (prog-out p))
  (for ([x insts] [x-vals access])
    (when (equal? M (inst-out x))
      (assert (equal? (car x-vals) expected)))))

#|
(define p (prog* 2 (list (op #'bvadd bvadd) (op #'bvor bvor) (op #'bvneg bvneg))))
(define t (trace* p))
(term->datum (trace-out t))
(asserts)
|#  
