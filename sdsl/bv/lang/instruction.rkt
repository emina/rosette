#lang s-exp rosette

(require (only-in racket struct map remove-duplicates) 
         "location.rkt")

(provide instruction? 
         instruction-id instruction-proc 
         instruction-inputs instruction-input instruction-in-values 
         instruction-output instruction-out-value
         new-instruction
         check/pre check/spec check/wfp check/conn assert/sbp)

; Represents an instruction within a fragment of instructions.
; The id fields holds a syntactic identifier of the procedure 
; contained in the proc field.  The proc field holds the procedure 
; executed by the instruction.
;
; The inputs list is a list of locations of other instructions whose
; output values are consumed by the given instruction.  The output 
; value is the location of the instruction within the fragment.
; The location datatype is described in locations.rkt.
;
; The in-values and out-value are the actual values read from / 
; assigned to the instruction locations.  Locations and values at 
; those locations are all integers (symbolic or concrete).
(struct instruction (id proc inputs output in-values out-value) 
  #:property prop:procedure 
  (lambda (self . args)
    (apply (instruction-proc self) args))
  #:property prop:custom-write 
  (lambda (self port mode) 
    (let ([recur (case mode
                   [(#t) write]
                   [(#f) display]
                   [else (lambda (p port) (print p port mode))])]
          [simplify (lambda (v) (if (term? v) (term->datum v) v))])
      (recur `(:= ,(simplify (instruction-output self)) 
                  (,(syntax->datum (instruction-id self)) 
                   ,@(map simplify (instruction-inputs self)))) port))))

; Returns the location from which the given instruction reads its ith input.
(define (instruction-input inst i)
  (list-ref (instruction-inputs inst) i))

; Creates and returns a new instruction that wraps the given 
; procedure identified by the given identifier.  The specified inputs list 
; holds the locations of instructions whose output values are consumed by the newly created 
; instruction, and the output is the location of the newly 
; created instruction within a sequence (or, fragment) of instructions.
; Locations are represented as symbolic or concrete integers.  Their 
; meaning within a fragment (sequence) of instructions is as described 
; in  Gulwani et al. "Synthesis of Loop-Free Programs", PLDI'11. 
(define (new-instruction id proc inputs output)
  (define arity (length inputs))
  (unless (procedure-arity-includes? proc arity)
    (error 'new-instruction "procedure ~a cannot be applied to ~a arguments" proc arity))
  (define in-values (for/list ([idx (in-range 0 arity)]) 
                      (local [(define-symbolic* in number?)]
                        in)))
  ;(log-info "~a ~a" id (map (compose1 cdr term-name) in-values))
  ; We ensure that each instruction's in/out values conform to its semantics.
  ; That is, out-value is the result of evaluating proc on in-values.  In 
  ; contrast to the original encoding in the paper cited above, this approach
  ; is more efficient, as it eliminates intermediate output variables, and the 
  ; additional assertions of the form (assert (= (apply proc in-values) out-value)).
  (instruction id proc inputs output in-values 
               (let-values ([(out asserts) (with-asserts (apply proc in-values))])
                 out))) 

; A simple symmetry breaking predicate that can be optionally asserted for a library 
; of instructions (with symbolic locations).
(define (assert/sbp #:library lib)
  (let* ([ids (remove-duplicates (map instruction-id lib))]
         [parts (map (lambda (id) (filter-map (lambda (inst) (and (equal? id (instruction-id inst)) inst)) lib)) ids)])
    (for ([p parts] #:unless (null? (cdr p)))
      (do ([i p (cdr i)]) ((or (null? i) (null? (cdr i))))
        (assert (location<? (instruction-output (car i)) (instruction-output (cadr i))))))))

; Checks that instruction preconditions (if any) are met.  We specify this by simply executing 
; each instruction on its input values, which may trigger assertions inside of 
; instructions procedure.  These will be captured by the callers of this function, if desired.
(define (check/pre #:library insts)
  (for ([x insts])
    (apply (instruction-proc x) (instruction-in-values x))))

; Takes as input a specification predicate, a list of (symbolic or concrete) 
; inputs and a library (list) of instructions, and checks that 
; the output value of the instruction at the (output) location (- M 1), 
; where M is (+ (length inputs) (length lib)), satisfies the spec.  That is,
; (spec inputs (instruction-out-value inst)) is true for the instruction inst 
; at the output location (- M 1).  
(define (check/spec #:spec spec #:inputs inputs #:library insts)
  (define O (location (sub1 (+ (length inputs) (length insts)))))
  (for ([x insts])
    (assert (=> (location=? (instruction-output x) O) 
                (apply spec `(,@inputs ,(instruction-out-value x)))))))

; Takes as input a list of (symbolic or concrete) bitvector 
; inputs and a library (list) of instructions, and checks that, 
; when the library instructions are sorted according to their 
; output location, they form a straight-line code fragment  over 
; the given inputs.  As in Gulwani et al. "Synthesis of Loop-Free Programs", 
; PLDI'11, locations are ordered, and we identify the ith input with 
; the location i.  The output locations of the instructions in the 
; given library must therefore be unique and drawn from (length inputs) to 
; (+ (length inputs) (length lib)), exclusive.  We further require 
; that the input locations for the library instructions obey the 
; SSA constraint:  the location of an instruction's input must 
; precede that of the instruction's output.  This procedure terminates 
; succesfully iff the given library of instructions is well-formed over
; the given inputs; otherwise it fails with an assertion violation.
(define (check/wfp #:inputs inputs #:library insts )
  (define I (length inputs))
  (define N (length insts))
  (define M (sub1 (+ I N))) 
                 
  ; Consistency: Every instruction has a unique output location.
  (do ([i insts (cdr i)]) ((null? i))
    (let ([x (car i)])
      (for ([y (cdr i)])
        (assert (not (location=? (instruction-output x) (instruction-output y)))))))
  
  ; Acyclicity: The location of every instruction's input precedes the location of its output.
  ; Location validity:  All input locations are between 0 and M, inclusive, and all
  ; output locations are between I and M, inclusive.
  (define-values (loc0 locI locM) 
    (values (location 0) (location I) (location M)))
  
  (for ([x insts])
    (let ([x-out (instruction-output x)])
      (for ([x-in (instruction-inputs x)])
        (assert (location<? x-in x-out))            ; acyclicity
        (assert (location<=? loc0 x-in))            ; input validity
        (assert (location<=? x-in locM)))           ; input validity  
      (assert (location<=? locI x-out))             ; output validity
      (assert (location<=? x-out locM))))           ; output validity
  )

; Takes as input a list of (symbolic or concrete) bitvector 
; inputs and a library (list) of instructions, and checks that  
; the instruction locations are consistent with the values read/written 
; by each instruction.  As in Gulwani et al. "Synthesis of Loop-Free Programs", 
; PLDI'11, locations are ordered, and we identify the ith input with 
; the location i.  We require the location 
; assignment to be consistent with instruction semantics:  applying 
; an instruction to the values drawn from the specified locations yields 
; the instruction's output value.  This procedure terminates 
; succesfully iff the given library of instructions is consistent over
; the given inputs; otherwise it fails with an assertion violation.
(define (check/conn #:inputs inputs #:library insts)
  (define I (length inputs)) 
  ; Connectivity: If the ith input location of an instruction x is the same as 
  ; the output location of instruction y, then the ith input value of x is the 
  ; the same as y's output value.  We consider the specification (global) inputs 
  ; to be nullary instructions with a single output.
  (for ([(out-val idx) (in-indexed inputs)])
    (let ([out-loc (location idx)])
      (for ([x insts])
        (for ([x-in-loc (instruction-inputs x)]
              [x-in-val (instruction-in-values x)])
          (assert (=> (location=? x-in-loc out-loc) (= x-in-val out-val)))))))
  
  (for ([y insts])
    (let ([out-loc (instruction-output y)]
          [out-val (instruction-out-value y)])
      (for ([x insts] #:unless (equal? x y))
        (for ([x-in-loc (instruction-inputs x)]
              [x-in-val (instruction-in-values x)])
          (assert (=> (location=? x-in-loc out-loc) (= x-in-val out-val))))))))
