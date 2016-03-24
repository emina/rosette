#lang rosette

(require "memory.rkt" "reals.rkt" "type.rkt" "pointers.rkt")

(provide current-memory malloc address-of)

; Models the current state of the execution.

; The current memory parameter holds the memory object 
; from which are currently allocating memory.  This may 
; be the host's memory or the device's global memory.
(define current-memory
  (make-parameter
   (memory)
   (lambda (m)
     (match m
       [(? memory? m) m]
       [m (raise-argument-error 'current-memory "memory" m)]))))

; A simplified model of malloc that takes as input the 
; number of cells to allocate, and where each scalar type 
; has the same size (1).  The size argument must be a concrete
; exact non-negative integer.
(define (malloc size)
  (unless (exact-nonnegative-integer? size)
    (raise-argument-error 'malloc "concrete exact nonnegative integer" size))
  (memory-allocate! (current-memory) size))

; Given an lvalue and the type of that lvalue, returns a 
; pointer object through which that value can be accessed 
; and mutated.  The type must be a real-type?.
(define-syntax-rule (address-of lval type)
  (local-pointer (lambda () ((type) lval)) 
                 (lambda (v) (set! lval ((type) v)))
                 #'lval
                 type))

(struct local-pointer (get set! address type)
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
    (fprintf port "#~x[~a]" (local-pointer-address self) ((local-pointer-get self))))]
  #:methods gen:pointer
  [(define (pointer-address self)      
     (local-pointer-address self))
   
   (define (pointer-size self)        
     (real-type-length (local-pointer-type self)))
   
   (define (pointer-cast self t) 
     (define type (local-pointer-type self))
     (unless (equal? t type)
       (raise-argument-error 'pointer-cast (~a (type-name type)) t))
     self)
   
   (define (pointer-ref self idx) 
     (unless (= idx 0)
       (raise-argument-error 'pointer-ref "0" idx))
     ((local-pointer-get self)))
   
   (define (pointer-set! self idx val)
     (unless (= idx 0)
       (raise-argument-error 'pointer-set! "0" idx))
     ((local-pointer-set! self) val))
     
   (define (pointer->list self)
     (list ((local-pointer-get self))))])
