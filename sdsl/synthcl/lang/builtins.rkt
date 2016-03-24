#lang rosette

(require (for-syntax (only-in rosette make-list)) (only-in racket/syntax format-symbol)
         "types.rkt" 
         "../model/memory.rkt" "../model/flags.rkt" "../model/work.rkt" 
         "../model/context.rkt" "../model/buffer.rkt" 
         "../model/program.rkt"  "../model/kernel.rkt"
         "../model/queue.rkt" "../model/runtime.rkt")

(provide builtins current-seconds current-milliseconds
         CL_MEM_READ_WRITE CL_MEM_READ_ONLY CL_MEM_WRITE_ONLY NULL
         malloc memset get_work_dim get_global_offset 
         get_global_size get_local_size get_num_groups
         get_global_id get_local_id get_group_id
         clCreateContext clCreateBuffer clCreateProgramWithSource clCreateKernel clSetKernelArg    
         clCreateCommandQueue clEnqueueReadBuffer clEnqueueWriteBuffer clEnqueueNDRangeKernel
         convert_int2 convert_int3 convert_int4 convert_int8 convert_int16 
         convert_float2 convert_float3 convert_float4 convert_float8 convert_float16)

(define int->int (function-type : int -> int))
 
(define-syntax (constructor-type stx)
  (syntax-case stx (: ->)
    [(_ : [base k] -> out)
     (quasisyntax/loc stx
       (function-type : #,@(make-list (syntax->datum #'k) #'base) -> out))]))

(define (is-a? t) 
  (if (type? t)
      (flat-named-contract (format-symbol "~a" (type-name t)) (curry equal? t))
      (flat-named-contract (object-name t) t)))

(define (real-type-of-length k)
  (flat-named-contract (format-symbol "real-type-of-length-~a" k)
                       (and/c real-type? (compose1 (curry equal? k) real-type-length))))

; Dicitionary from identifiers of built-in functions to their types.
(define builtins
  (hash 
   
   ; constants
   #'CL_MEM_READ_WRITE  int
   #'CL_MEM_READ_ONLY   int
   #'CL_MEM_WRITE_ONLY  int
   #'NULL               void*
   
   ; c procedures
   #'malloc            (function-type : int -> void*)
   #'memset            (function-type : void* int int -> void*)
   #'current-seconds   (function-type : -> int)
   #'current-milliseconds (function-type : -> int)
   
   ; work procedures
   #'get_work_dim      (function-type : -> int)
   #'get_global_offset int->int
   #'get_global_size   int->int
   #'get_local_size    int->int
   #'get_num_groups    int->int
   #'get_global_id     int->int
   #'get_local_id      int->int
   #'get_group_id      int->int
   
   ; api procedures
   #'clCreateContext           (function-type : -> cl_context)
   #'clCreateBuffer            (function-type : cl_context int int -> cl_mem)
   #'clCreateProgramWithSource (function-type : cl_context char* -> cl_program)
   #'clCreateKernel            (function-type : cl_program char* -> cl_kernel)
   #'clSetKernelArg            (function-type : cl_kernel int (or/c (is-a? cl_mem) (is-a? real-type?)) -> void)
   #'clCreateCommandQueue      (function-type : cl_context -> cl_command_queue)
   #'clEnqueueReadBuffer       (function-type : cl_command_queue cl_mem int int void* -> void)
   #'clEnqueueWriteBuffer      (function-type : cl_command_queue cl_mem int int void* -> void)
   #'clEnqueueNDRangeKernel    (function-type : cl_command_queue cl_kernel int int* int* int* -> void)
   
   ; vector value constructors
   #'int2    (constructor-type : [int 2] -> int2)
   #'int3    (constructor-type : [int 3] -> int3)
   #'int4    (constructor-type : [int 4] -> int4)
   #'int8    (constructor-type : [int 8] -> int8)
   #'int16   (constructor-type : [int 16] -> int16)   
   #'float2  (constructor-type : [float 2] -> float2)
   #'float3  (constructor-type : [float 3] -> float3)
   #'float4  (constructor-type : [float 4] -> float4)
   #'float8  (constructor-type : [float 8] -> float8)
   #'float16 (constructor-type : [float 16] -> float16)
   
   ; vector value converters
   #'convert_int2  (function-type : (real-type-of-length 2) -> int2)
   #'convert_int3  (function-type : (real-type-of-length 3) -> int3)
   #'convert_int4  (function-type : (real-type-of-length 4) -> int4)
   #'convert_int5  (function-type : (real-type-of-length 8) -> int8)
   #'convert_int16 (function-type : (real-type-of-length 16) -> int16)
   
   #'convert_float2  (function-type : (real-type-of-length 2) -> float2)
   #'convert_float3  (function-type : (real-type-of-length 3) -> float3)
   #'convert_float4  (function-type : (real-type-of-length 4) -> float4)
   #'convert_float5  (function-type : (real-type-of-length 8) -> float8)
   #'convert_float16 (function-type : (real-type-of-length 16) -> float16)))
  


