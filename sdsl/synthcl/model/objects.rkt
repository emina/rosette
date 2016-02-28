#lang rosette

(require "type.rkt" "context.rkt" "queue.rkt" "program.rkt" "kernel.rkt" "buffer.rkt"  
         rosette/lib/match)

(provide cl-type? cl_context cl_command_queue cl_program cl_kernel cl_mem)

; This module provides a common base for the representation of 
; opaque OpenCL object types (e.g., cl_mem).

(struct cl-type (base procedure)
  #:property prop:procedure
  [struct-field-index procedure]
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (object-name self)))]
  #:methods gen:type
  [(define (type-name self) (object-name self))
   (define (type-base self) (cl-type-base self))])

; A macro for defining an opaque type from a Rosette base predicate.
(define-syntax-rule (define-cl-type id base)
  (define id
    (let* ([cast (match-lambda [(? base v) v]
                               [other (raise-argument-error 'id (symbol->string 'id) other)])]
           [id   (case-lambda [() cast]
                              [args (error 'id "not an instance constructor")])])
      (cl-type base id))))

(define-cl-type cl_mem buffer?)
(define-cl-type cl_context context?)
(define-cl-type cl_command_queue command-queue?)
(define-cl-type cl_program program?)
(define-cl-type cl_kernel kernel?)


     

  
