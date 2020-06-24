#lang rosette

(require "machine.rkt" "indistinguishable.rkt" "verify.rkt"
         "basic.rkt" "jump.rkt" "call.rkt")

(require rackunit rackunit/text-ui rosette/lib/roseunit rosette/solver/smt/boolector)

(when (boolector-available?)
  (current-solver (boolector)))

; See verify-EENI-demo.rkt for details on the results expected by the tests.

(define (cex-case name ended? prog [k #f])
  (test-case name (check-pred EENI-witness? (verify-EENI* init ended? mem≈ prog k))))

(define (valid-case name ended? prog [k #f])
  (test-case name (check-true (verify-EENI* init ended? mem≈ prog k))))

(define-syntax-rule (define-tests id desc expr ...)
  (define id
    (test-suite+ 
     desc 
     (begin expr ...))))

; Checks for counterexamples for bugs in basic semantics. 
(define-tests basic-bugs "IFC: counterexamples for bugs in basic semantics"
  (cex-case "Fig. 1" halted? (program 3 (list Halt Noop Push Pop Add* Load* Store*AB))) 
  (cex-case "Fig. 2" halted? (program 3 (list Halt Noop Push Pop Add* Load* Store*B)))  
  (cex-case "Fig. 3" halted? (program 5 (list Halt Noop Push Pop Add* Load* Store)))  
  (cex-case "Fig. 4" halted? (program 7 (list Halt Noop Push Pop Add Load* Store))))  

(define-tests basic-correct "IFC: no bounded counterexamples for correct basic semantics"
  (valid-case "*" halted? (program 7 (list Halt Noop Push Pop Add Load Store)))  
  (valid-case "+" halted? (program 8 (list Halt Noop Push Pop Add Load Store))))  

(define-tests jump-bugs "IFC: counterexamples for bugs in jump+basic semantics"
  (cex-case "11" halted? (program 6 (list Halt Noop Push Pop Add Load Store Jump*AB)))  
  (cex-case "12" halted? (program 4 (list Halt Noop Push Pop Add Load Store Jump*B)))) 

(define-tests jump-correct "IFC: no bounded counterexamples for correct jump+basic semantics"
  (valid-case "**" halted? (program 7 (list Halt Noop Push Pop Add Load Store Jump)))    
  (valid-case "++" halted? (program 8 (list Halt Noop Push Pop Add Load Store Jump))))    

(define-tests call-return-bugs "IFC: counterexamples for buggy call+return+basic semantics"
  (cex-case "Fig. 13" halted∩low? (program 7 (list Halt Noop Push Pop Add Load Store Call*B Return*AB)))
  (cex-case "Fig. 15" halted∩low? (program 8 (list Halt Noop Push Pop Add Load StoreCR Call*B Return*AB)))
  (cex-case "Fig. 16" halted∩low? (program 8 (list Halt Noop Push Pop Add Load StoreCR Call*B Return*B)))
  (cex-case "Fig. 17" halted∩low? (program 10 (list Halt Noop Push Pop Add Load StoreCR Call Return))))

(define-tests reproduce-bugs "IFC: counterexamples that are structurally similar to those in prior work"
  (cex-case "Fig. 13*" halted∩low? (program (list Push Call*B Halt Push Push Store Return*AB)))
  (cex-case "Fig. 15*" halted∩low? (program (list Push Push Call*B Push StoreCR Halt Push Return*AB)))
  (cex-case "Fig. 16*" halted∩low? (program (list Push Push Call*B Push StoreCR Halt Return*B Push Return*B)))
  (cex-case "Fig. 17*" 
            halted∩low? 
            (program (list Push Call Push StoreCR Halt Push Push Call Pop Push Return))
            13))

(define (fast-tests)
  (time (run-tests basic-bugs))       ; ~10 sec
  (time (run-tests basic-correct))    ; ~20 sec
  (time (run-tests jump-bugs)))       ; ~7 sec

(define (slow-tests)
  (time (run-tests jump-correct))     ; ~52 sec
  (time (run-tests call-return-bugs)) ; ~440 sec
  (time (run-tests reproduce-bugs)))  ; ~256 sec

(module+ fast
  (fast-tests))

(module+ test
  (fast-tests)
  (slow-tests))



