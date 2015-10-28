#lang racket

(provide 
 op? op-name 
 
 ;; --- phasing in --- ;; 
 define-operator lifted-op?
 (rename-out 
  [lifted-op-safe op-safe]
  [lifted-op-unsafe op-unsafe])  

 ;; --- phasing out --- ;;  
 define-op typed-op? op/->                    
 op-arg-type op-out-type   
 (rename-out 
  [typed-op-pre op-pre]))     

(struct op 
  (name h1 h2)  
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "~a" (op-name self)))]
  #:methods gen:equal+hash
  [(define (equal-proc op1 op2 eq-proc) (eq? op1 op2))
   (define (hash-proc op1 hash-proc) (op-h1 op1))
   (define (hash2-proc op2 hash-proc) (op-h2 op2))])

;; ------ Lifted ops (phasing in) ------ ;;

; By default, op application uses the safe (lifted) version.  This version 
; performs type checking on the arguments, and asserts the preconditions, if any, 
; before calling the unsafe version of the operator.  The unsafe version is used 
; internally by Rosette for efficiency.  It assumes that all of its arguments are 
; properly typed and that all preconditions are met.
(struct lifted-op op
  (safe unsafe type)  
  #:property prop:procedure 
  (struct-field-index safe)) 

(define (make-lifted-op #:safe safe #:unsafe unsafe #:type type #:name [name (object-name unsafe)] )
  (let ([str-name (symbol->string name)])
    (lifted-op 
     name (equal-hash-code str-name) (equal-secondary-hash-code str-name)
     safe unsafe type)))

(define-syntax-rule (define-operator id arg ...)
  (define id (make-lifted-op arg ...)))
    

;; ------ Typed ops (to be phased out) ------ ;;

(struct typed-op op
  (arg out pre proc)  
  #:property prop:procedure 
  (struct-field-index proc))


(define (make-typed-op id #:name [name (syntax->datum id)] #:type type #:pre [pre (const #t)] #:op proc) 
  (let ([str-name (symbol->string name)])
    (typed-op 
     name (equal-hash-code str-name) (equal-secondary-hash-code str-name)
     (car type) (cdr type) pre proc)))

(define-syntax (define-op stx)
  (syntax-case stx ()
    [(_ id args ...) #`(define id (make-typed-op #'id args ...))]))

(define (op-arg-type op idx)
  ((typed-op-arg op) idx))

(define (op-out-type op args) 
  (match op
    [(typed-op _ _ _ _ t _ _) (apply t args)]
    [(lifted-op _ _ _ _ _ t)  (apply t args)]))
    
    

; Creates an operator type specification using the given argument and result types.
; If arg-types is just one type, then the specification corresponds to an
; arbitrary arity relation over that type.  If the argument is a list of k types, 
; then the specification encodes the given k-ary relation over those k types.
(define-syntax (op/-> stx)
  (syntax-case stx ()
    [(_ (#:rest rest-type) out-type)
     #'(cons (lambda (i) rest-type)
             (lambda xs out-type))]
    [(_ (arg-type ... #:rest rest-type) out-type)
     (with-syntax ([(idx ...) (build-list (length (syntax->list #'(arg-type ...))) values)])
       #'(cons (lambda (i)
                 (case i 
                   [(idx) arg-type] ...
                   [else rest-type]))
               (lambda xs out-type)))]
    [(_ (arg-type ...) out-type) 
     (with-syntax ([(idx ...) (build-list (length (syntax->list #'(arg-type ...))) values)])
       #'(cons (lambda (i)
                 (case i 
                   [(idx) arg-type] ...
                   [else (error 'op-arg-type "Index out of bounds: ~a" i)])) 
               (lambda xs out-type)))]
    [(_ #:arg-type arg-type-proc #:out-type out-type-proc)
     #'(cons arg-type-proc out-type-proc)]))
