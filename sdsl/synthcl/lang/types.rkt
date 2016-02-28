#lang rosette

; Provides a mapping from OpenCL type identifiers to 
; the type instances / predicates in the OpenCL model.

(require (only-in racket/syntax format-symbol)
         syntax/id-table rosette/lib/synthax
         (for-syntax (only-in "../model/type.rkt" type-name type?))
         "../model/type.rkt" "../model/reals.rkt" 
         "../model/pointers.rkt" "../model/objects.rkt")

(provide type-ref type-set type-identifier? identifier->type 
         base->real-type base->pointer-type
         check-implicit-conversion check-no-conversion
         function-type? function-type-args function-type-result 
         (rename-out [make-function-type function-type]) 
         char* (all-from-out "../model/type.rkt"  "../model/reals.rkt" 
                             "../model/pointers.rkt" "../model/objects.rkt"))
         


; String type is needed for typechecking of string literals passed 
; as arguments to some functions, but strings are not supported in 
; any other way in the model.
(struct shallow-type (name base)
  #:property prop:procedure
  (lambda (self)
    (lambda (arg)
      (if ((shallow-type-base self) arg)
          arg
          (raise-argument-error (shallow-type-name self) 
                                (object-name (shallow-type-base self)) 
                                arg))))
  #:methods gen:type
  [(define (type-name self) (shallow-type-name self))
   (define (type-base self) (shallow-type-base self))]
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (shallow-type-name self)))])

(define char* (shallow-type 'char* string?))

(define types 
  (let ([types (list char* void bool int float int2 int3 int4 int8 int16 
                     float2 float3 float4 float8 float16 
                     void* bool* int* float* int2* int3* int4* int8* int16* 
                     float2* float3* float4* float8* float16*
                     cl_context cl_command_queue cl_program cl_kernel cl_mem)]
        [tbl (make-free-id-table #:phase 10)])
    (for ([t types])
      (dict-set! tbl #`#,(type-name t) t))
    tbl)) 

(begin-for-syntax 
  (save-properties
   (lambda (stx) 
     (let ([t (syntax-property stx 'type)])
       (and t (object-name t))))))

(restore-properties
 (lambda (stx t) 
   (let ([t (and t (dict-ref types #`#,t (thunk #f)))])
     (if t (type-set stx t) stx))))

; Returns the type of the given expression of #f if it has no 
; type annotation.
(define (type-ref stx)
  (syntax-property stx 'type))

; Returns a copy of stx with its type set to t.
(define (type-set stx t)
  (syntax-property stx 'type t))

; Returns the type object corresponding to the given identifier.
(define (identifier->type id context)
  (dict-ref types id (thunk (raise-syntax-error #f "not a valid type identifier" context id))))

; Returns the real type object with the given base and length.
(define (base->real-type base length)
  (if (= length 1)
      (dict-ref types #`#,(type-name base))
      (dict-ref types #`#,(format-symbol "~a~a" (type-name base) length))))

; Returns the pointer type object with the given base.
(define (base->pointer-type base)
  (dict-ref types #`#,(format-symbol "~a*" (type-name base))))

; Returns true iff the given identifier is a valid type identifier.
(define (type-identifier? id)
  (and (identifier? id) (dict-has-key? types id)))

; A function type is modeled by a struct with two fields: the args field 
; and the result field.  The args field is a list of k types or 
; predicates that take a type, which specifies that all procedures with 
; the given function type accept exactly k arguments with the specified 
; types or types accepted by the specified predicates. The result field 
; stores a single type, which is the type of the function's output.
(struct function-type (args result))

; Returns a function-type for the given result type and 
; a list of argument types (if any).  
(define-syntax make-function-type 
  (syntax-rules (: ->)
    [(_ : arg ... -> result)
     (function-type (list arg ...) result)]
    [(_ args result)
     (function-type args result)]))

; Checks if an instance of the "from" type can be cast to an instance of the 
; "to" type, if "to" is a type.  If "to" is a predicate rather than a type, 
; then the procedure checks if that predicate accepts the "from" type.  Otherwise 
; throws a syntax error, using the given context syntaxes for localization.
(define (check-implicit-conversion from to expr [subexpr #f])
  (unless (if (type? to)
              (or (equal? from to)
                  (and (scalar-type? from) (scalar-type? to))
                  (and (scalar-type? from) (vector-type? to))
                  (and (pointer-type? from) (pointer-type? to))
                  (and (equal? from cl_mem) (pointer-type? to)))
              (to from))
      (raise-syntax-error 
       #f 
       (format "no implicit conversion from ~a to ~a" from (if (contract? to) (contract-name to) to))
       expr subexpr)))

; Checks that the "from" and "to" types are equal.  Otherwise 
; throws a syntax error, using the given context syntaxes for localization.
(define (check-no-conversion from to expr [subexpr #f])
  (unless (equal? from to)
    (raise-syntax-error #f (format "expected ~a, given ~a" to from) expr subexpr)))
          

         



