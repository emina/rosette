#lang s-exp "../../../lang/main.rkt"

; Matrix multiplication C = A * B, where A is an n x p matrix and B is a p x m matrix.
(kernel void (mmulScalarKernel [int* A] [int* B] [int* C] [int p] [int m])
  (: int i j sum)
  (= i (get_global_id 0))
  (= j (get_global_id 1))
  (= sum 0)
  (for [(: int k in (range p))]
    (+= sum (* [A (+ (* i p) k)] [B (+ (* k m) j)])))
  (= [C (+ (* i m) j)] sum))

;;--------------- Vectorized kernel ---------------;;

; Matrix multiplication C = A * B, where A is an n x p matrix and B is a p x m matrix.
(kernel void (mmulVectorKernel [int4* A] [int4* B] [int4* C] [int p] [int m])
  (: int i j)
  (: int4 sum0 sum1 sum2 sum3)
  
  (= i (get_global_id 0))
  (= j (get_global_id 1))
  (= sum0 0)
  (= sum1 0)
  (= sum2 0)
  (= sum3 0)
  
  (for [(: int k in (range 0 p 4))]
    (: int4 a0 a1 a2 a3)
    (: int4 b0 b1 b2 b3)
    
    (= a0 [A (indexA 0 i k p)])
    (= a1 [A (indexA 1 i k p)])
    (= a2 [A (indexA 2 i k p)])
    (= a3 [A (indexA 3 i k p)])
    
    (= b0 [B (indexB 0 k j m)])
    (= b1 [B (indexB 1 k j m)])
    (= b2 [B (indexB 2 k j m)])
    (= b3 [B (indexB 3 k j m)])
    
    (+= sum0 (computeSum a0 b0 b1 b2 b3))
    (+= sum1 (computeSum a1 b0 b1 b2 b3))
    (+= sum2 (computeSum a2 b0 b1 b2 b3))
    (+= sum3 (computeSum a3 b0 b1 b2 b3)))
     
  (= [C (indexC 0 i j m)] sum0)
  (= [C (indexC 1 i j m)] sum1)
  (= [C (indexC 2 i j m)] sum2)
  (= [C (indexC 3 i j m)] sum3))
  
; Multiplies the 1x4 vector a by the 4x4 matrix with rows b0, b1, b2 and b3.
(procedure int4 (computeSum [int4 a] [int4 b0] [int4 b1] [int4 b2] [int4 b3])
  (int4 
    (+ (* [a x] [b0 x]) (* [a y] [b1 x]) (* [a z] [b2 x]) (* [a w] [b3 x]))
    (+ (* [a x] [b0 y]) (* [a y] [b1 y]) (* [a z] [b2 y]) (* [a w] [b3 y]))
    (+ (* [a x] [b0 z]) (* [a y] [b1 z]) (* [a z] [b2 z]) (* [a w] [b3 z]))
    (+ (* [a x] [b0 w]) (* [a y] [b1 w]) (* [a z] [b2 w]) (* [a w] [b3 w]))))

(procedure int (indexA [int off] [int i] [int k] [int p])
  (+ (* (+ (* i 4) off) (/ p 4)) (/ k 4)))

(procedure int (indexB [int off] [int k] [int j] [int m])
  (+ (* (+ k off) (/ m 4)) j))

(procedure int (indexC [int off] [int i] [int j] [int m])
  (+ (* (+ (* i 4) off) (/ m 4)) j))


;;; ---------------- Optimized kernel implementation transcribed from AMD's apps ---------------- ;;;
(: int TILEX TILEX_SHIFT TILEY TILEY_SHIFT)
(= TILEX 4)
(= TILEY_SHIFT 2)
(= TILEY 4)
(= TILEY_SHIFT 2)
 
; Matrix multiplication C = A * B, where A is an n x p matrix and B is an 
; p x m matrix.
(kernel void (mmulVectorKernelOpt [int4* A] [int4* B] [int4* C] [int p] [int m])
  (: int2 pos)
  (: int4 sum0 sum1 sum2 sum3)
  
  (= pos (int2 (get_global_id 0) (get_global_id 1)))
  (= sum0 0)
  (= sum1 0)
  (= sum2 0)
  (= sum3 0)
  
  (/= m 4)

  (for [(: int i in (range 0 p 4))]
    (: int4 a0 a1 a2 a3)
    (: int4 b0 b1 b2 b3)
    
    (= a0 [A (+ (/ i 4) (* (<< [pos x] TILEY_SHIFT) (/ p 4)))])
    (= a1 [A (+ (/ i 4) (* (+ (<< [pos x] TILEY_SHIFT) 1) (/ p 4)))])
    (= a2 [A (+ (/ i 4) (* (+ (<< [pos x] TILEY_SHIFT) 2) (/ p 4)))])
    (= a3 [A (+ (/ i 4) (* (+ (<< [pos x] TILEY_SHIFT) 3) (/ p 4)))])
    
    (= b0 [B (+ [pos y] (* i m))])
    (= b1 [B (+ [pos y] (* (+ i 1) m))])
    (= b2 [B (+ [pos y] (* (+ i 2) m))])
    (= b3 [B (+ [pos y] (* (+ i 3) m))])
    
    (+= [sum0 x] (+ (* [a0 x] [b0 x]) (* [a0 y] [b1 x]) (* [a0 z] [b2 x]) (* [a0 w] [b3 x])))
    (+= [sum0 y] (+ (* [a0 x] [b0 y]) (* [a0 y] [b1 y]) (* [a0 z] [b2 y]) (* [a0 w] [b3 y])))
    (+= [sum0 z] (+ (* [a0 x] [b0 z]) (* [a0 y] [b1 z]) (* [a0 z] [b2 z]) (* [a0 w] [b3 z])))
    (+= [sum0 w] (+ (* [a0 x] [b0 w]) (* [a0 y] [b1 w]) (* [a0 z] [b2 w]) (* [a0 w] [b3 w])))
    
    (+= [sum1 x] (+ (* [a1 x] [b0 x]) (* [a1 y] [b1 x]) (* [a1 z] [b2 x]) (* [a1 w] [b3 x])))
    (+= [sum1 y] (+ (* [a1 x] [b0 y]) (* [a1 y] [b1 y]) (* [a1 z] [b2 y]) (* [a1 w] [b3 y])))
    (+= [sum1 z] (+ (* [a1 x] [b0 z]) (* [a1 y] [b1 z]) (* [a1 z] [b2 z]) (* [a1 w] [b3 z])))
    (+= [sum1 w] (+ (* [a1 x] [b0 w]) (* [a1 y] [b1 w]) (* [a1 z] [b2 w]) (* [a1 w] [b3 w])))
    
    (+= [sum2 x] (+ (* [a2 x] [b0 x]) (* [a2 y] [b1 x]) (* [a2 z] [b2 x]) (* [a2 w] [b3 x])))
    (+= [sum2 y] (+ (* [a2 x] [b0 y]) (* [a2 y] [b1 y]) (* [a2 z] [b2 y]) (* [a2 w] [b3 y])))
    (+= [sum2 z] (+ (* [a2 x] [b0 z]) (* [a2 y] [b1 z]) (* [a2 z] [b2 z]) (* [a2 w] [b3 z])))
    (+= [sum2 w] (+ (* [a2 x] [b0 w]) (* [a2 y] [b1 w]) (* [a2 z] [b2 w]) (* [a2 w] [b3 w])))
    
    (+= [sum3 x] (+ (* [a3 x] [b0 x]) (* [a3 y] [b1 x]) (* [a3 z] [b2 x]) (* [a3 w] [b3 x])))
    (+= [sum3 y] (+ (* [a3 x] [b0 y]) (* [a3 y] [b1 y]) (* [a3 z] [b2 y]) (* [a3 w] [b3 y])))
    (+= [sum3 z] (+ (* [a3 x] [b0 z]) (* [a3 y] [b1 z]) (* [a3 z] [b2 z]) (* [a3 w] [b3 z])))
    (+= [sum3 w] (+ (* [a3 x] [b0 w]) (* [a3 y] [b1 w]) (* [a3 z] [b2 w]) (* [a3 w] [b3 w]))))
  
  (= [C (+ [pos y] (* (+ (<< [pos x] TILEY_SHIFT) 0) m))] sum0)
  (= [C (+ [pos y] (* (+ (<< [pos x] TILEY_SHIFT) 1) m))] sum1)
  (= [C (+ [pos y] (* (+ (<< [pos x] TILEY_SHIFT) 2) m))] sum2)
  (= [C (+ [pos y] (* (+ (<< [pos x] TILEY_SHIFT) 3) m))] sum3))