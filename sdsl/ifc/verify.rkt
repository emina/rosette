#lang s-exp rosette

(require "machine.rkt" "indistinguishable.rkt")

(provide verify-EENI verify-SSNI)

; Verifies the EENI[start, end, ≈] property for machines executing 
; k steps of a program that is indistinguishable from the given program.  
; The start argument is a procedure that takes as input a program and 
; returns a fresh machine m initialized with that program.  The pc, 
; stack and memory of every machine m0 and m1 returned by start must be 
; indistinguishable. The end argument is a predicate on machines, and 
; ≈ is an indistinguishability relation on machines.  The length 
; of the program list is used as the default value for the number of steps.
; (-> (-> (listof instruction?) machine?)
;     (-> machine? boolean?)
;     (-> machine? machine? boolean?)
;     (listof instruction?)
;     [number?]
;     void?)
(define (verify-EENI start end ≈ prog [k (length prog)])
  (printf "\n-------Verify EENI[~a, ~a, ~a] for ~a steps of ~s-------\n"
          (object-name start) (object-name end) (object-name ≈) k (format-program prog))
  
  (define t0 (current-milliseconds))

  (define m0 (start prog))
  (define m1 (start (map instruction (map procedure prog)))) ; Use a fresh program (same procedures, fresh args).
  
  (define cex 
    (with-handlers ([exn:fail? (lambda (e) #f)]) ; The verify form throws an exception if no cex is found.
      (verify 
       #:assume  (begin 
                   (assert (≈ m0 m1))     
                   (set! m0 (step m0 k))  
                   (set! m1 (step m1 k)) 
                   (assert (end m0))
                   (assert (end m1)))
       #:guarantee (assert (≈ m0 m1)))))
  
  (define t (- (current-milliseconds) t0))
  (print-cex cex t (cons "m0" m0) (cons "m1" m1)))
               
; Returns a printable representation of the given program.
(define (format-program prog)
  (match prog
    [(list (instruction (? union? r) _) _ ...)        
     (sort (map object-name (union-values r)) string<? #:key symbol->string)]
    [(list (instruction (? procedure? proc) _) ...) 
     (map object-name proc)]))

; Prints the given counterexample for the specified label/machine pairs.
(define (print-cex cex t . labeled-machines)
  (cond [cex  (printf "Counterexample found (~a ms).\n" t)
              (for ([label/machine labeled-machines])
                (printf "~a:\n~s\n" (car label/machine) (evaluate (cdr label/machine) cex)))]
        [else (printf "No counterexample found (~a ms).\n" t)]))

;; ------ Ignore SSNI implementation (not sure what it should be) ------ ;;

; Verifies the SSNI[all, full≈] property for machines executing 
; 1 step of two indisitinguishable machines executing a single
; instruction that performs the given procedure.  The S and M arguments 
; determine the initial size of the machines' stack and memory, 
; respectively.  Both arguments are 2 by default.
; (-> (-> machine? value? ... machine?) [number?] [number?] void?)
(define (verify-SSNI proc [S 2] [M 2])
  (printf "\n-------Verify SSNI[All, full≈] for ~s with stack depth ~s and memory size ~a-------\n"
          (object-name proc) S M)
  (define t0 (current-milliseconds))
  
  (define m0 (all (list (instruction proc)) S M))
  (define m1 (all (list (instruction proc)) S M))
  (define m01 m0)
  (define m11 m1)
  
  (define cex
    (with-handlers ([exn:fail? (lambda (e) #f)]) ; The verify form throws an exception if no cex is found.
      (verify 
       #:assume    (begin 
                     (assert (full≈ m0 m1))                   ; Assume that m0 and m1 are full≈, and 
                     (set! m01 (step m0 1))                   ; that each takes one step to states m01 and m11, 
                     (set! m11 (step m1 1))                   ; respectively.
                     (assert (equal? (@label (pc m01))        ; m01 and m11 must be either both high or both low.
                                     (@label (pc m11))))
                     (assert (not (eq? m0 m01)))              ; Neither m0 nor m1 is stuck.
                     (assert (not (eq? m1 m11))))
       #:guarantee (let@ ([(pc0 Lpc0) (pc m0)]
                          [(pc01 Lpc01) (pc m01)]
                          [(pc1 Lpc1) (pc m1)]
                          [(pc11 Lpc11) (pc m11)])
                         (cond [(equal? Lpc0 ⊥)               ; If m0 and m1 started in low states, their end
                                (assert (full≈ m01 m11))]     ; states must be full≈.
                               [(equal? Lpc01 ⊥)              ; If m0 and m1 ended in low states, their end
                                (assert (full≈ m01 m11))]     ; those end states must be full≈. 
                               [else          
                                (assert (full≈ m0 m01))       ; If m0 and m1 ended in high states, then the ending 
                                (assert (full≈ m1 m11))]))))) ; state for each is full≈ to its starting state.
  
  (define t (- (current-milliseconds) t0))
  (print-cex cex t (cons "m0" m0) (cons "m1" m1) (cons "(step m0)" m01)  (cons "(step m1)" m11)))
