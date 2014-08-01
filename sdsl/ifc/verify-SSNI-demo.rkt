#lang s-exp rosette

(require "machine.rkt" "indistinguishable.rkt" "verify.rkt"
         "basic.rkt" "jump.rkt" "call.rkt")

; See the full draft of "Testing Noninterference, Quickly" for 
; a description of the SSNI verification problem for IFC machine 
; semantics.
;
; The basic semantics (Section 2) consists of various buggy and 
; correct implementations of the instructions Halt, Noop, Push,
; Pop, Load, Store and Add for machines operating on labeled values.
; These are defined in basic.rkt, and their names correspond 
; to those in "Testing Noninterference, Quickly."
;
; Control flow semantics (Section 5) includes several versions of 
; three additional instructions:  Jump, Call and Return.  The 
; Jump instruction and variants are defined in jump.rkt and Call/Return
; are defined in call.rkt.
;
; In this module, we are using the verify-SSNI form (see verify.rkt)
; to check that all indistinguishable machines executing indistinguishable 
; one-instruction programs for 1 step end in states that satisfy the SSNI
; property.  The expression (verify-SSNI proc S M) verifies the SSNI[all, full≈]
; property for machines with S stack elements and M memory cells, executing 
; programs with an instruction that performs the given procedure. The default value 
; for S and M, if they are not provided, is 2. If the verifier finds 2 machines 
; violating SSNI (i.e., machines with indistinguishable start states and end states 
; that violate SSNI), it pretty prints both their initial and final states.  


; Uncomment the following two lines to use Z3 instead of Kodkod:
;(require rosette/solver/z3/z3)
;(current-solver (new z3%))

; Shows counterexamples for bugs in basic semantics. 
(define (basic-bugs)
  #|1|# (verify-SSNI Store*AB) 
  #|2|# (verify-SSNI Store*B)  
  #|3|# (verify-SSNI Add*)  
  #|4|# (verify-SSNI Load*))

; Shows counterexamples for bugs in jump+basic semantics. 
(define (jump-bugs [≈ mem≈])
  #|11|# (verify-SSNI Jump*AB)  
  #|12|# (verify-SSNI Jump*B)) 

; Shows counterexamples to buggy call+return+basic semantics.  
(define (call-return-bugs [≈ mem≈])
  #|13|# (verify-SSNI Return*AB)
  #|15|# (verify-SSNI Return*B)
  #|16|# (verify-SSNI Call*B))

