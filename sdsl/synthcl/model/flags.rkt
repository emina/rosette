#lang rosette

(provide (all-defined-out))

; Defines various OpenCL flags.

(define-syntax-rule (define-flags [flag val] ...)
  (define-values (flag ...) (values val ...)))

(define-flags 
  [CL_MEM_READ_WRITE 1] 
  [CL_MEM_WRITE_ONLY 2]
  [CL_MEM_READ_ONLY 4])
