#lang s-exp "../../../lang/main.rkt"
     
; Compute the number of steps for the algorithm, 
; assuming that v is a power of 2.  See the log2 
; algorithm from http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog
(procedure int (steps [int v])
  (: int r)
  (= r 0)
  ($= r (<< (!= 0 (& v #xAAAAAAAA)) 0))
  ($= r (<< (!= 0 (& v #xCCCCCCCC)) 1))
  ($= r (<< (!= 0 (& v #xF0F0F0F0)) 2))
  ($= r (<< (!= 0 (& v #xFF00FF00)) 3))
  ($= r (<< (!= 0 (& v #xFFFF0000)) 4))
  r)

; Reference implementation for Fast Walsh Transform.  This implementation
; requires the length of the input array to be a power of 2, and it modifies
; the input array in place.
(procedure float* (fwt [float* tArray] [int length])
  (for [(: int i in (range 0 (steps length)))]
    (: int step)
    (= step (<< 1 i))
    (for [(: int group in (range 0 step))
          (: int pair  in (range group length (<< step 1)))]
      (: int match)
      (: float t1 t2)
      (= match (+ pair step))
      (= t1 [tArray pair])
      (= t2 [tArray match])
      (= [tArray pair]  (+ t1 t2))
      (= [tArray match] (- t1 t2))))
  tArray)

; Scalar host for Fast Walsh Transform.   This implementation
; requires the length of the input array to be a power of 2.  The 
; input array is not modified; the output is a new array that holds
; the result of the transform.
(procedure float* (fwtScalarHost [float* input] [int length])
  (: cl_context context)
  (: cl_command_queue command_queue)
  (: cl_program program)
  (: cl_kernel kernel)
  (: cl_mem tBuffer)
  (: float* tArray)
  (: int dim global)
  
  (= dim (* length (sizeof float)))
  (= global (/ length 2))
  
  (= tArray ((float*) (malloc dim)))
  
  (= context (clCreateContext))
  
  (= command_queue (clCreateCommandQueue context))
 
  (= tBuffer (clCreateBuffer context CL_MEM_READ_WRITE dim))
  (= program (clCreateProgramWithSource context "kernel.rkt"))
  
  (clEnqueueWriteBuffer command_queue tBuffer 0 dim input)
  
  (= kernel (clCreateKernel program "fwtKernelSketch"))
  (clSetKernelArg kernel 0 tBuffer)
  
  (for [(: int i in (range 0 (steps length)))]
    (: int step)
    (= step (<< 1 i))
    (clSetKernelArg kernel 1 step)
    (clEnqueueNDRangeKernel command_queue kernel 1 NULL (@ global) NULL))
  
  (clEnqueueReadBuffer command_queue tBuffer 0 dim tArray)
  tArray)

; Vectorized host for Fast Walsh Transform.   This implementation
; requires the length of the input array to be a power of 2.  The 
; input array is not modified; the output is a new array that holds
; the result of the transform.
(procedure float* (fwtVectorHost [float* input] [int length])
  (: cl_context context)
  (: cl_command_queue command_queue)
  (: cl_program program)
  (: cl_mem tBuffer)
  (: float* tArray)
  (: int dim global n)
  
  (= dim (* length (sizeof float)))
  (= global (/ length 2))
  
  (= tArray ((float*) (malloc dim)))
  
  (= context (clCreateContext))
  
  (= command_queue (clCreateCommandQueue context))
 
  (= tBuffer (clCreateBuffer context CL_MEM_READ_WRITE dim))
  (= program (clCreateProgramWithSource context "kernel.rkt"))
  
  (clEnqueueWriteBuffer command_queue tBuffer 0 dim input)
  
  (= n (steps length))
  
  (runKernel command_queue (clCreateKernel program "fwtKernel") tBuffer global 0 (?: (< n 2) n 2))
  (if (> n 2) 
      { (/= global 4)
        (runKernel command_queue (clCreateKernel program "fwtKernel4Sketch") tBuffer global 2 n) })

  (clEnqueueReadBuffer command_queue tBuffer 0 dim tArray)
  tArray)

(procedure void (runKernel [cl_command_queue command_queue] [cl_kernel kernel] [cl_mem tBuffer] 
                           [int global] [int start] [int end])
  (clSetKernelArg kernel 0 tBuffer)
  (for [(: int i in (range start end))]
    (: int step)
    (= step (<< 1 i))
    (clSetKernelArg kernel 1 step)
    (clEnqueueNDRangeKernel command_queue kernel 1 NULL (@ global) NULL)))
  
; Given two arrays of the same size, checks that they hold the same 
; values at each index.
(procedure void (check [int* actual] [int* expected] [int SIZE])
  (assert (>= SIZE 0))
  (for [(: int i in (range SIZE))]
    (assert (== [actual i] [expected i]))))

(procedure void (synth_scalar)  ; ~7 sec
  (synth #:forall [(: int length in (range 8 9))
                   (: float[length] tArray)]
         #:ensure (check (fwtScalarHost tArray length)
                         (fwt tArray length)
                         length)))

(procedure void (synth_vector) ; < 1 sec
  (synth #:forall [(: int length in (range 8 9))
                   (: float[length] tArray)]
         #:ensure (check (fwtVectorHost tArray length)
                         (fwt tArray length)
                         length)))
       

