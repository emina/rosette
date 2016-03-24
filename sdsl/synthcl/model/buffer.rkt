#lang rosette

(require "flags.rkt" "type.rkt" "pointers.rkt" "memory.rkt" "context.rkt" "work.rkt"
         rosette/lib/match)

(provide clCreateBuffer buffer? buffer-context buffer-copy!)

; Creates a new buffer object using the given context flags and size.  
; Note that this is a simplified interface to the opencl-1.2 
; specification (Ch. 5.2.1) and only limited functionality is exposed.
(define (clCreateBuffer context flags size)
  (or (equal? flags CL_MEM_READ_WRITE) 
      (equal? flags CL_MEM_READ_ONLY) 
      (equal? flags CL_MEM_WRITE_ONLY)
      (raise-argument-error 'buffer "a valid kernel access flag" flags))
  (buffer context (memory-allocate! (context-global-memory context) size) flags))

; Allocates a buffer in the given memory that has the same size, 
; access control flags, and data as the given buffer.
(define (buffer-copy! mem buff)
  (match buff
    [(buffer context ptr flags)
     (buffer context (memory-allocate! mem (pointer-size ptr) ptr) flags)]
    [_ 
     (raise-argument-error 'buffer-copy! "buffer" buff)]))

(define (buffer-address self)
  (pointer-address (buffer-pointer self)))

(define (buffer-size self) 
  (pointer-size (buffer-pointer self)))

(define (buffer-cast self type) 
  (buffer (buffer-context self)
          (pointer-cast (buffer-pointer self) type) 
          (buffer-flags self)))

(define (buffer->list self) 
  (pointer->list (buffer-pointer self)))

(define (buffer-ref self idx)
  (define ptr (buffer-pointer self))
  (if (and (in-kernel?) (equal? (buffer-flags self) CL_MEM_WRITE_ONLY))
      (error 'pointer-ref "cannot read from a write-only memory address #x~x[~a]" 
                           (pointer-address ptr) idx)
      (pointer-ref ptr idx)))

(define (buffer-set! self idx val) 
  (define ptr (buffer-pointer self))
  (if (and (in-kernel?) (equal? (buffer-flags self) CL_MEM_READ_ONLY))
      (error 'pointer-set! "cannot write to a read-only memory address #x~x[~a]" 
                           (pointer-address ptr) idx)
      (pointer-set! (buffer-pointer self) idx val)))

; OpenCL buffers are modeled as objects that 
; store a pointer to memory, context in which they
; are create, and flags that determine whether a 
; given access operation (a read or a write) is permitted 
; on a particular buffer object.
(struct buffer (context pointer flags)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (buffer-pointer self)))]
  #:methods gen:pointer
  [(define pointer-address buffer-address) 
   (define pointer-size buffer-size)
   (define pointer-cast buffer-cast)
   (define pointer-ref buffer-ref)
   (define pointer-set! buffer-set!)
   (define pointer->list buffer->list)])

