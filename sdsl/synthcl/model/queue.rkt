#lang rosette

(require "context.rkt" "buffer.rkt" "kernel.rkt" 
         "work.rkt" "runtime.rkt" "memory.rkt"
         "pointers.rkt" "errors.rkt" 
         rosette/lib/match)

(provide clCreateCommandQueue clEnqueueReadBuffer clEnqueueWriteBuffer clEnqueueNDRangeKernel command-queue?)

; Models the OpenCL command queue for a given context.
(struct command-queue (context [tasks #:auto #:mutable])
  #:auto-value '()
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "#<command-queue:~a>" (eq-hash-code self)))])

; Creates a command queue in the given context and returns it.
; Our model does not support out of order execution. See Ch. 5.2.2 
; of opencl-1.2 specification for the full interface.
(define (clCreateCommandQueue context)
  (unless (context? context)
    (raise-argument-error 'clCreateCommandQueue "context" context))
  (command-queue context))

; Enqueues a command to read from a buffer object to host memory.  All 
; reads are implicitly blocking.  The command queue and the buffer must 
; have the same context. See Ch. 5.2.2 of opencl-1.2 specification 
; for the full interface.
(define (clEnqueueReadBuffer cq buff offset size ptr)
  (define context (command-queue-context cq))
  (unless (eq? context (buffer-context buff))
    (raise-context-error 'clEnqueueReadBuffer cq "buffer" buff))
  (assert (memory-owns? (current-memory) ptr)
          (thunk (raise-argument-error 'clEnqueueReadBuffer "valid pointer to host memory" ptr)))
  (define global-memory (context-global-memory context))
  (for ([i (in-range offset (+ offset size))])
    (pointer-set! ptr i (pointer-ref buff i)))
  (void))

; Enqueues a command to write from host memory to a buffer object.  All 
; reads are implicitly blocking.  The command queue and the buffer must 
; have the same context.  See Ch. 5.2.2 of opencl-1.2 specification 
; for the full interface.
(define (clEnqueueWriteBuffer cq buff offset size ptr)
  (define context (command-queue-context cq))
  (unless (eq? context  (buffer-context buff))
    (raise-context-error 'clEnqueueWriteBuffer cq "buffer" buff))
  (assert (memory-owns? (current-memory) ptr)
          (thunk (raise-argument-error 'clEnqueueWriteBuffer "valid pointer to host memory" ptr)))
  (define global-memory (context-global-memory context))
  (for ([i (in-range offset (+ offset size))])
    (pointer-set! buff i (pointer-ref ptr i)))
  (memory-synchronize! global-memory)
  (void))

; Enqueus a command to  execute a kernel on a device.  The command queue 
; and the kernel must have the same context.  See Ch. 5.8 of opencl-1.2 
; specification for the full interface.
(define (clEnqueueNDRangeKernel cq kernel work-dim global-work-offset global-work-size local-work-size)
  (define context (command-queue-context cq))
  (unless (eq? (command-queue-context cq) (kernel-context kernel))
    (raise-context-error 'clEnqueueNDRangeKernel cq "kernel" kernel))
  (unless (> work-dim 0)
    (raise-argument-error 'clEnqueueNDRangeKernel "positive work dimension" work-dim))
  (define global-memory (context-global-memory context))
  (define gws (pointer->list global-work-size))
  (define work-items (apply * gws))
  (parameterize ([current-work-size (list (pointer->list global-work-offset) gws (pointer->list local-work-size))]
                 [current-memory global-memory])
    (for ([id (apply * gws)])
      (parameterize ([current-global-id id])
        (kernel))))
  (memory-synchronize! global-memory)
  (void))

        


    
