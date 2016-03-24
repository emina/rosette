#lang rosette

(require "machine.rkt" "indistinguishable.rkt" "verify.rkt"
         "basic.rkt" "jump.rkt" "call.rkt")

; See the full draft of "Testing Noninterference, Quickly" for 
; a description of the EENI verification problem for IFC machine 
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
; In this module, we are using the verify-EENI form (see verify.rkt)
; to check that all indistinguishable machines executing indistinguishable 
; programs for n steps end in indistinguishable states.  The expression 
; (program k procs) constructs a symbolic program with k instructions, 
; where each instruction may execute any of the given procedures. The
; expression (verify-EENI start end ≈ prog n) verifies the EENI[start, end, ≈]
; property for machines executing programs that are indistinguishable
; from prog.  If it finds 2 machines violating EENI (i.e., machines with 
; indistinguishable start states and distinguishable end states), it pretty 
; prints their final states.  The final state of each machine must be 
; reached in n steps.  If n is not specified, the default value of 
; n = (length prog) is used.

; Shows counterexamples for bugs in basic semantics. 
;
;     mem≈ (sec)    |
; ------------------|
; (Fig. 1) z3 = 0.5 |
; (Fig. 2) z3 = 0.4 |
; (Fig. 3) z3 = 2.9 |
; (Fig. 4) z3 = 12  |
; ------------------|
(define (basic-bugs [≈ mem≈])
  #|1|# (verify-EENI init halted? ≈ (program 3 (list Halt Noop Push Pop Add* Load* Store*AB))) 
  #|2|# (verify-EENI init halted? ≈ (program 3 (list Halt Noop Push Pop Add* Load* Store*B)))  
  #|3|# (verify-EENI init halted? ≈ (program 5 (list Halt Noop Push Pop Add* Load* Store)))  
  #|4|# (verify-EENI init halted? ≈ (program 7 (list Halt Noop Push Pop Add Load* Store))))  

; Shows absence of bounded counterexamples for correct basic semantics.
;
;  mem≈ (sec)   |
; --------------|
; (*) z3 = 12   |
; (+) z3 = 24   |
; --------------|
(define (basic-correct [≈ mem≈])
  #|*|# (verify-EENI init halted? ≈ (program 7 (list Halt Noop Push Pop Add Load Store)))  
  #|+|# (verify-EENI init halted? ≈ (program 8 (list Halt Noop Push Pop Add Load Store))))  

; Shows counterexamples for bugs in jump+basic semantics.  Note that these are quite different
; from the corresponding counterexamples in the paper.
;
;     mem≈ (sec)     | 
; -------------------| 
; (Fig. 11) z3 = 7.7 | 
; (Fig. 12) z3 = 1.8 | 
; -------------------|
(define (jump-bugs [≈ mem≈])
  #|11|# (verify-EENI init halted? ≈ (program 6 (list Halt Noop Push Pop Add Load Store Jump*AB)))  
  #|12|# (verify-EENI init halted? ≈ (program 4 (list Halt Noop Push Pop Add Load Store Jump*B)))) 

; Shows absence of bounded counterexamples for correct jump+basic semantics.
;
;   mem≈ (sec)  | 
; --------------| 
; (*) z3 = 21   |  
; (+) z3 = 52   | 
; --------------|
(define (jump-correct [≈ mem≈])
  #|*|# (verify-EENI init halted? ≈ (program 7 (list Halt Noop Push Pop Add Load Store Jump)))    
  #|+|# (verify-EENI init halted? ≈ (program 8 (list Halt Noop Push Pop Add Load Store Jump))))    

; Shows counterexamples to buggy call+return+basic semantics. Note that call/return uses a different
; EENI property (with halted∩low?), as in the paper.  We find different (some shorter) counterexamples. 
;
;     mem≈ (sec)     | 
; -------------------| 
; (Fig. 13) z3 = 37  |   
; (Fig. 15) z3 = 90  |
; (Fig. 16) z3 = 62  |
; (Fig. 17) z3 = 492 |
; -------------------|
(define (call-return-bugs [≈ mem≈])
  #|13|# (verify-EENI init halted∩low? ≈ (program 7 (list Halt Noop Push Pop Add Load Store Call*B Return*AB)))
  #|15|# (verify-EENI init halted∩low? ≈ (program 8 (list Halt Noop Push Pop Add Load StoreCR Call*B Return*AB)))
  #|16|# (verify-EENI init halted∩low? ≈ (program 8 (list Halt Noop Push Pop Add Load StoreCR Call*B Return*B)))
  #|17|# (verify-EENI init halted∩low? ≈ (program 10 (list Halt Noop Push Pop Add Load StoreCR Call Return))))

; Shows absence of bounded counterexamples for correct call/retrn + modified basic semantics.
;
;     mem≈ (sec)  | 
; ----------------| 
; (*) z3 > 1200   |   
; ----------------|
(define (call-return-correct [≈ mem≈])
  #|*|# (verify-EENI init halted∩low? ≈ (program 10 (list Halt Noop Push PopCR Add Load StoreCR Call Return))))
  
; Confirms that we can find counterexamples that are structurally similar to those in the paper.  Note that
; the expression (program procs) constructs a program that consists of (length procs) instructions, 
; where the ith instruction executes the ith procedure in procs.
(define (reproduce-bugs)
  #|13|# (verify-EENI init halted∩low? mem≈ 
                      (program (list Push Call*B Halt Push Push Store Return*AB)))
  #|15|# (verify-EENI init halted∩low? mem≈ 
                      (program (list Push Push Call*B Push StoreCR Halt Push Return*AB)))
  #|16|# (verify-EENI init halted∩low? mem≈ 
                      (program (list Push Push Call*B Push StoreCR Halt Return*B Push Return*B)))
  #|17|# (verify-EENI init halted∩low? mem≈ 
                      (program (list Push Call Push StoreCR Halt Push Push Call Pop Push Return))
                      13))







#|
; See comment in call.rkt on the implementation of Return.  There is a 
; bug in the paper revealed by the counterexample found below:
; (verify-EENI init halted∩low? mem≈ (program 7 (list Halt Noop Push Pop Add Load StoreCR Call Return)))
(define p0
  (list (instruction Push (@ 5 ⊥))
        (instruction Call (@ 0 ⊥) (@ 1 ⊥))
        (instruction Push (@ 1 ⊥))
        (instruction StoreCR)
        (instruction Halt)
        (instruction Push (@ 2 ⊤))
        (instruction Return)))

(define p1
  (list (instruction Push (@ 5 ⊥))
        (instruction Call (@ 0 ⊥) (@ 1 ⊥))
        (instruction Push (@ 1 ⊥))
        (instruction StoreCR)
        (instruction Halt)
        (instruction Push (@ 6 ⊤))
        (instruction Return)))

(define m0 (init p0))
(define m1 (init p1))

(for ([i (length p0)])
  (printf "STEP ~a\nm0: ~s\nm1: ~s\n" i m0 m1)
  (set! m0 (step m0 1))
  (set! m1 (step m1 1)))

(printf "STEP ~a\nm0: ~s\nm1: ~s\n" (length p0) m0 m1)|#
  

#|
; Fig. 17 cex.

(define p0
  (list (instruction Push (@ 5 ⊥))
        (instruction Call (@ 0 ⊥) (@ 1 ⊥))
        (instruction Push (@ 0 ⊥))
        (instruction StoreCR)
        (instruction Halt)
        (instruction Push (@ 0 ⊥))
        (instruction Push (@ 8 ⊤))
        (instruction Call (@ 0 ⊥) (@ 0 ⊥))
        (instruction Pop)
        (instruction Push (@ 0 ⊥))
        (instruction Return)))

(define p1
  (list (instruction Push (@ 5 ⊥))
        (instruction Call (@ 0 ⊥) (@ 1 ⊥))
        (instruction Push (@ 0 ⊥))
        (instruction StoreCR)
        (instruction Halt)
        (instruction Push (@ 0 ⊥))
        (instruction Push (@ 9 ⊤))
        (instruction Call (@ 0 ⊥) (@ 0 ⊥))
        (instruction Pop)
        (instruction Push (@ 0 ⊥))
        (instruction Return)))
        
(define m0 (init p0))
(define m1 (init p1))

(define k 13)

(for ([i k])
  (printf "STEP ~a\nm0: ~s\nm1: ~s\n" i m0 m1)
  (set! m0 (step m0 1))
  (set! m1 (step m1 1)))

(printf "STEP ~a\nm0: ~s\nm1: ~s\n" k m0 m1)|#

