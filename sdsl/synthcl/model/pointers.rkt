#lang rosette

(require "type.rkt" "reals.rkt" "errors.rkt" 
         (for-syntax (only-in racket/syntax format-id)))

(provide pointer-type? 
         gen:pointer pointer? pointer-address pointer-size 
         pointer-cast pointer->list pointer-ref pointer-set!
         void* bool* int* float*
         int2* int3* int4* int8* int16* 
         float2* float3* float4* float8* float16*)

; A pointer object points to a storage region, which is a
; a one-dimensional array of elements.  The elements 
; of this collection can be primitive scalars or vectors.
; Our model of pointers (and memory) is simplified in that 
; we assume all datatypes have the same size (in bytes).
(define-generics pointer  
  
  ; Returns the base address of the given pointer in memory.
  [pointer-address pointer]
  
  ; Returns the length of the region pointed to by the given pointer.
  [pointer-size pointer]
  
  ; Returns a view of the given pointer that treats its contents 
  ; as having the specified type T.  In particular, 
  ; pointer-ref and pointer-set! operations on the returned pointer 
  ; will produce and consume only values of type T. For example,
  ; (pointer-set! p i v) to a pointer p that has been cast to the 
  ; int3 type will implicitly convert t to int3 before performing 
  ; the update.  Similarly, (pointer-ref p i) will return a value of 
  ; type int3.
  [pointer-cast pointer type]
  
  ; Returns the value stored at the given offset from the 
  ; base address of the given pointer (i.e., ptr[idx]).
  [pointer-ref pointer idx]
  
  ; Sets the memory cell at the given offset from the 
  ; base address of the given pointer to the given value 
  ; (i.e., ptr[idx] := val).
  [pointer-set! pointer idx val]
  
  ; Returns a list view of the storage region for the given pointer.
  [pointer->list pointer])

; A simple model of OpenCL pointer types. 
; A pointer type is the type of an OpenCL pointer.
; It consists of a name and a primitive type, which 
; which is the type of the elements in a pointer instance.
; 
; Each pointer type T is also procedure that takes no 
; arguments and produces another procedure.  The returned 
; procedure, when applied to a pointer value  
; casts it to a value of type T.  This is an unchecked cast.
(struct pointer-type (base procedure)
  #:property prop:procedure
  [struct-field-index procedure]
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (object-name self)))]
  #:methods gen:type
  [(define (type-name self) (object-name self))
   (define (type-base self) (pointer-type-base self))])


(define void*
  (pointer-type 
   void 
   (let* ([cast-void* (lambda (b) 
                        (assert (pointer? b) (format "pointer-cast: cannot cast ~a to ~a*" b void))
                        b)]
          [void* (thunk cast-void*)])
     void*)))
                                         

(define-syntax (define-pointer-type stx)
  (syntax-case stx ()
    [(_ base)
     (with-syntax ([id (format-id #'base "~a*" #'base #:source #'base)])
       (syntax/loc stx
         (define id
           (pointer-type
            base
            (let* ([convert (curryr pointer-cast base)]
                   [id (thunk convert)])
              id)))))]
    [(_ base ...)
     (syntax/loc stx
       (begin
         (define-pointer-type base) ...))]))

(define-pointer-type
  bool int float
  int2 int3 int4 int8 int16 
  float2 float3 float4 float8 float16)




#|           
(global-memory (memory))
(define b (memory-allocate! (global-memory) CL_MEM_READ_WRITE 10))
(define bb* ((bool*) b))
(pointer-ref bb* 5)
(pointer-set! bb* 5 #t)
(pointer-set! bb* 6 10)
(pointer-ref bb* 5)|#
