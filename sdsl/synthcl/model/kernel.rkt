#lang rosette

(require (only-in racket/syntax format-symbol) "context.rkt" "buffer.rkt" "program.rkt" "errors.rkt")

(provide clCreateKernel clSetKernelArg kernel? kernel-context)

; Models a kernel procedure as an applicable 
; struct instance with a context and a vector of arguments.
; A kernel instance accepts no arguments.  When 
; invoked, it applies kernel-procedure to kernel-args.
(struct kernel (context args procedure)
  #:property prop:procedure
  [struct-field-index procedure]
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (cons (object-name self) 
                              (vector->list (kernel-args self)))))])

; Creates and returns a kernel for the given procedure from the 
; specified program. See Ch. 5.7 of opencl-1.2 specification for 
; the full interface.
(define (clCreateKernel program kernel-name)
  (define proc (cdr (or (assoc kernel-name (program-kernels program))
                        (raise-argument-error 'clCreateKernel "valid kernel name" kernel-name))))
  (define arity (procedure-arity proc))
  (unless (integer? arity)
    (raise-argument-error 
     'kernel 
     "procedure that accepts an exact number of arguments" proc))
  (define args (make-vector arity (void)))
  (kernel (program-context program) 
          args (procedure-rename
                (thunk (apply proc (vector->list args)))
                (format-symbol "kernel:~a" (object-name proc)))))

; Sets the kernel argument at the given index to the given value.
; Valid values are memory objects and primitive values.  If a memory 
; object is provided, its data is copied and the val pointer can be 
; reused by the host after clSetKernelArg returns.  This is a simplified 
; model of the interface in which the size of the value argument need 
; not be provided (since local memory is not modeled).
; See Ch. 5.7.2 of opencl-1.2 specification.
(define (clSetKernelArg kernel idx val)
  (match val
    [(? buffer? buff)
     (if (eq? (buffer-context buff) (kernel-context kernel))
         (vector-set! (kernel-args kernel) idx buff)
         (raise-context-error 'clSetKernelArg kernel "buffer" buff))]
    [datum 
     (if (type-of datum)
         (vector-set! (kernel-args kernel) idx datum)
         (raise-argument-error 'clSetKernelArg "memory object or real value" datum))]))
 
  
