#lang rosette

(require "memory.rkt" rosette/lib/match)

(provide clCreateContext context? 
         context-global-memory current-context)
        
(define current-context 
  (make-parameter 
   #f
   (lambda (p)
     (match p
       [(? context? p) p]
       [p (raise-argument-error 'current-context "context" p)]))))

; Models the OpenCL context for a device.  Contexts are 
; used by the OpenCL runtime for managing objects such as 
; command-queues, memory, program and kernel objects and for 
; executing kernels on one or more devices specified in the context.
; See Ch. 4.4 of opencl-1.2 specification.
;
; In this simplified model of contexts, devices are 
; left unspecified, and work items all share a single global memory.
; Local memory is not supported.
(struct context (global-memory))

; Creates an OpenCL context. Contexts are used by the OpenCL 
; runtime for managing objects such as command-queues, memory, 
; program and kernel objects and for executing kernels on one or 
; more devices.  Our model leaves the devices unspecified.   
; For the full interface, see Ch. 4.4 of opencl-1.2 specification.
(define (clCreateContext)
  (context (memory)))


