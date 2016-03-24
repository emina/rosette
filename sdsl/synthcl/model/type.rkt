#lang rosette

(provide type? type-name type-base gen:type)

; Provides a common interface for all structures 
; that double as OpenCL types.

; A type has a name and, optionally, a base type.  The base type
; for a pointer type P is the OpenCL scalar or vector 
; type to which instances of P point.  The base type for 
; a vector type V is the scalar type whose members 
; comprise an instance of V.  The base type for a scalar 
; type or an opaque OpenCL type (e.g., cl_kernel) is a Rosette 
; predicate that recognizes the instances of that scalar type.   
(define-generics type  
  [type-name type]
  [type-base type])



