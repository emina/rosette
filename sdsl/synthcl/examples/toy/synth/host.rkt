#lang s-exp "../../../lang/main.rkt"

; Given a src array of length SIZE, returns a new array dst of 
; the same size, such that dst[i] = src[i] + offset, for all 
; 0 <= i < SIZE.  The SIZE parameter must be evenly divisible by 4.
(procedure int* (host [int* src] [int SIZE] [int offset])
  (: cl_context context)
  (: cl_command_queue command_queue)
  (: cl_program program)
  (: cl_kernel kernel)
  (: cl_mem buffer_src buffer_dst)
  (: int* dst)
  (: int global)

  (= global (/ SIZE 4))
  
  (= dst ((int*) (malloc (* SIZE (sizeof int)))))
  
  (= context (clCreateContext))
  
  (= command_queue (clCreateCommandQueue context))
 
  (= buffer_src (clCreateBuffer context CL_MEM_READ_ONLY (* SIZE (sizeof int))))
  (= buffer_dst (clCreateBuffer context CL_MEM_WRITE_ONLY (* SIZE (sizeof int))))
  
  (= program (clCreateProgramWithSource context "kernel.rkt"))
  
  (clEnqueueWriteBuffer command_queue buffer_src 0 SIZE src)
  
  (= kernel (clCreateKernel program "sample"))
  (clSetKernelArg kernel 0 buffer_dst)
  (clSetKernelArg kernel 1 buffer_src)
  (clSetKernelArg kernel 2 offset)

  (clEnqueueNDRangeKernel command_queue kernel 1 NULL (@ global) NULL)
  (clEnqueueReadBuffer command_queue buffer_dst 0 SIZE dst)
  dst)

; The reference implementation for the host procedure. 
; Given a src array of length SIZE, returns a new array dst of 
; the same size, such that dst[i] = src[i] + offset, for all 
; 0 <= i < SIZE.  The SIZE parameter must be evenly divisible by 4.
(procedure int* (spec [int* src] [int SIZE] [int offset])
  (: int* dst)
  (= dst ((int*) (malloc (* SIZE (sizeof int)))))
  (for [(: int i in (range SIZE))]
;    (= [dst i] (?: (&& (== SIZE 16) (== offset -1)) 
;                   (- [src i] offset) 
;                   (+ [src i] offset)))) ; sample bug
    (= [dst i] (+ [src i] offset)))
  dst)
  
; Given two arrays of the same size, checks that they hold the same 
; values at each index.
(procedure void (check [int* actual] [int* expected] [int SIZE])
  (assert (>= SIZE 0))
  (for [(: int i in (range SIZE))]
    (assert (== [actual i] [expected i]))))

; Synthesize the kernel procedure so that it is correct for all small 
; arrays of length that is evenly divisible by 4.  In this case, we are 
; ensuring correctness for all arrays of size 4, 8, 12, 16 and 20.
(synth #:forall [(: int offset)
                 (: int len in (range 4 21 4))
                 (: int[len] src)] 
       #:ensure (check (host src len offset) (spec src len offset) len))
