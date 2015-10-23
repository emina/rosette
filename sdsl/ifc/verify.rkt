#lang s-exp rosette

(require "machine.rkt" "indistinguishable.rkt")

(provide verify-EENI)

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


