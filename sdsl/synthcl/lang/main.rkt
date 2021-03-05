#lang rosette

(require (for-syntax "typecheck.rkt" "types.rkt" 
                     (only-in rosette in-dict dict-keys)
                     (only-in "sugar.rkt" desugar)
                     (only-in "env.rkt" bind current-env [env make-env]))
         "types.rkt" "operators.rkt" "forms.rkt" "queries.rkt"
         "builtins.rkt" "sugar.rkt")

(provide (for-syntax #%datum = >= > < <= - +)
         (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [app-or-ref #%app])
         #%top #%datum  ;#%app
         
         ; Real types
         void bool int float int2 int3 int4 int8 int16 
         float2 float3 float4 float8 float16         
         ; Pointer types
         void* bool* int* float* int2* int3* int4* int8* int16* 
         float2* float3* float4* float8* float16* char*
         ; OpenCL types
         cl_context cl_command_queue cl_program cl_kernel cl_mem
         
         ; Procedures and kernels
         procedure kernel
         
         ; Statements and expressions
         : (rename-out [if-statement if] [for-statement for])  
         locally-scoped range @ sizeof 
         
         ; Solver-aided statements and forms
         assert verify synth choose ?? grammar
         
         ; Real operators
         = += -= *= /= %= &= $= ^= <<= >>=
         ?: ! && || + - * / % pow sqrt abs 
         & $ ^ ~ << >> == != < > <= >=
         
         ; Built-in constants and functions
         print current-seconds current-milliseconds
         CL_MEM_READ_WRITE CL_MEM_READ_ONLY CL_MEM_WRITE_ONLY NULL
         malloc memset get_work_dim get_global_offset 
         get_global_size get_local_size get_num_groups
         get_global_id get_local_id get_group_id
         clCreateContext clCreateBuffer clCreateProgramWithSource 
         clCreateKernel clSetKernelArg    
         clCreateCommandQueue 
         clEnqueueReadBuffer clEnqueueWriteBuffer clEnqueueNDRangeKernel
         convert_int2 convert_int3 convert_int4 convert_int8 convert_int16 
         convert_float2 convert_float3 convert_float4 convert_float8 convert_float16)
         
         

; This module parses, typechecks and transforms
; programs in the OpenCL DSL.
(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ body ...)
     (let-values ([(forms env) (typecheck-module (map desugar (syntax->list #'(body ...))))])
       (quasisyntax/loc stx
         (#%module-begin;#%plain-module-begin
          (provide #,@(dict-keys env))
          (begin-for-syntax 
            (current-env (make-env))
            #,@(env->bindings env))
          #,@(for/list ([form forms])
               (if (equal? void (type-ref form))
                   form
                   (quasisyntax/loc form (print #,form "\n")))))))]))
               
(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ form ...) (typecheck (desugar #'(form ...)))]
    [(_ . val) #'val]))

(begin-for-syntax
  (define (env->bindings env)
    (for/list ([(id t) (in-dict env)])
      (if (function-type? t)
          (quasisyntax/loc id 
            (bind #'#,id (function-type (list #,@(map type-name (function-type-args t)))  
                                        #,(type-name (function-type-result t)))))
          (quasisyntax/loc id
            (bind #'#,id #,(type-name t)))))))



