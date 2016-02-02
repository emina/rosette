#lang racket

(provide 
 op? op-name op-safe op-unsafe 
 define-operator op-out-type)    


; By default, an op application uses the safe (lifted) version of the operation.  
; This version performs type checking on the arguments, and asserts the preconditions, if any, 
; before calling the unsafe version of the operator.  The unsafe version is used 
; internally by Rosette for efficiency.  It assumes that all of its arguments are 
; properly typed and that all preconditions are met.
(struct op 
  (name h1 h2 safe unsafe type)  
  #:property prop:procedure 
  (struct-field-index safe)
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "~s" (op-name self)))]
  #:methods gen:equal+hash
  [(define (equal-proc op1 op2 eq-proc) (eq? op1 op2))
   (define (hash-proc op1 hash-proc) (op-h1 op1))
   (define (hash2-proc op2 hash-proc) (op-h2 op2))])

(define (make-op #:unsafe unsafe #:safe [safe unsafe] #:type type #:name [name (object-name unsafe)] )
  (let ([str-name (format "~s" name)]) 
    (op 
     (string->symbol str-name) 
     (equal-hash-code str-name) 
     (equal-secondary-hash-code str-name)
     safe unsafe type)))

(define-syntax-rule (define-operator id arg ...)
  (define id (make-op arg ...)))
    

(define (op-out-type operator args) 
  (match operator
    [(op _ _ _ _ _ t)  (apply t args)]))