#lang rosette

(require "machine.rkt" "indistinguishable.rkt")

(provide verify-EENI verify-EENI* (struct-out EENI-witness))

; Verifies the EENI[start, end, ≈] property for machines executing 
; k steps of a program that is indistinguishable from the given program.  
; The start argument is a procedure that takes as input a program and 
; returns a fresh machine m initialized with that program.  The pc, 
; stack and memory of every machine m0 and m1 returned by start must be 
; indistinguishable. The end argument is a predicate on machines, and 
; ≈ is an indistinguishability relation on machines.  The length 
; of the program list is used as the default value for the number of steps.
; (-> (-> (-> () (listof instruction?)) machine?)
;     (-> machine? boolean?)
;     (-> machine? machine? boolean?)
;     (listof instruction?)
;     [(or/c integer? #f)]
;     void?)
(define (verify-EENI start end ≈ prog [k #f])
  (time (displayln (verify-EENI* start end ≈ prog k #t))))

; Verifies the EENI[start, end, ≈] property for machines executing 
; k steps of a program that is indistinguishable from the given program.  
; The start argument is a procedure that takes as input a program and 
; returns a fresh machine m initialized with that program.  The pc, 
; stack and memory of every machine m0 and m1 returned by start must be 
; indistinguishable. The end argument is a predicate on machines, and 
; ≈ is an indistinguishability relation on machines.  The length 
; of the program list is used as the default value for the number of steps.
; The procedure returns a counterexample, if any, that the EENI[start, end, ≈] 
; property is violated by producing an EENI-witness.  If there are no 
; counterexample, the procedure returns #t.
; (-> (-> (-> () (listof instruction?)) machine?)
;     (-> machine? boolean?)
;     (-> machine? machine? boolean?)
;     (listof instruction?)
;     [(or/c integer? #f)]
;     (or/c EENI-witness? #t))
(define (verify-EENI* start end ≈ prog [k #f] [verbose? #f])
  (with-terms
    (parameterize ([current-bitwidth 5])
    
      (define p (prog))
    
      (unless k 
        (set! k (length p)))
    
      (when verbose?
        (printf "\n-------Verify EENI[~a, ~a, ~a] for ~a steps of ~s-------\n"
                (object-name start) (object-name end) (object-name ≈) k (format-program p)))

      (define m0 (start p))
      (define m1 (start (map instruction (map procedure p)))) ; Use a fresh program (same procedures, fresh args).
    
      (define cex
        (result-value
         (with-vc
             (let* ([m0k (step m0 k)]
                    [m1k (step m1 k)])
               (verify
                (begin
                  (assume (≈ m0 m1))
                  (assume (end m0k))
                  (assume (end m1k))
                  (assert (≈ m0k m1k))))))))
       
    
      (if (sat? cex) (EENI-witness (evaluate m0 cex) (evaluate m1 cex) k ≈) #t))))

(struct EENI-witness (m0 m1 k ≈)
  #:transparent 
  #:guard (lambda (m0 m1 k ≈ id)
            (unless (and (≈ m0 m1) (not (≈ (step m0 k) (step m1 k))))
              (error id "Not a valid EENI witness: ~a, ~a, ~a, ~a." m0 m1 k ≈))
            (values m0 m1 k ≈))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-let ([(EENI-witness m0 m1 k _) self])
        (fprintf port "m0 initial:\n~s\n" m0)
        (fprintf port "m0 final:\n~s\n" (step m0 k))
        (fprintf port "m1 initial:\n~s\n" m1)
        (fprintf port "m1 final:\n~s\n" (step m1 k))))])
        
     
; Returns a printable representation of the given program.
(define (format-program prog)
  (match prog
    [(list (instruction (? union? r) _) _ ...)        
     (sort (map object-name (union-values r)) string<? #:key symbol->string)]
    [(list (instruction (? procedure? proc) _) ...) 
     (map object-name proc)]))
