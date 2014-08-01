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

; Function for indexing into the matrix A.   
(procedure int (indexA [int off] [int i] [int k] [int p])
  ; (+ (* (+ (* i 4) off) (/ p 4)) (/ k 4))) 
  (: int r c w)
  (= r (+ (choose i (/ i 4) (* i 4)) (choose off 0)))
  (= c (+ (choose k (/ k 4) (* k 4)) (choose off 0)))
  (= w (+ (choose p (/ p 4) (* p 4)) (choose off 0)))
  (+ (* r w) c))

; Function for indexing into the matrix B.
(procedure int (indexB [int off] [int k] [int j] [int m])
  ;(+ (* (+ k off) (/ m 4)) j))
  (: int r c w)
  (= r (+ (choose k (/ k 4) (* k 4)) (choose off 0)))
  (= c (+ (choose j (/ j 4) (* j 4)) (choose off 0)))
  (= w (+ (choose m (/ m 4) (* m 4)) (choose off 0)))
  (+ (* r w) c))

; Function for indexing into the matrix C. 
(procedure int (indexC [int off] [int i] [int j] [int m])
  ;(+ (* (+ (* i 4) off) (/ m 4)) j))
  (: int r c w)
  (= r (+ (choose i (/ i 4) (* i 4)) (choose off 0)))
  (= c (+ (choose j (/ j 4) (* j 4)) (choose off 0)))
  (= w (+ (choose m (/ m 4) (* m 4)) (choose off 0)))
  (+ (* r w) c))
           

; Bad sketch and completions:
; Function for indexing into the matrix A.   
;(procedure int (indexA [int off] [int i] [int k] [int p])
  ; (+ (* (+ (/ i 4) off) (/ p 4)) (/ k 4))) 
  ;(: int r c w)
  ;(= r (+ (choose i (/ i 4)) (choose off 0)))
  ;(= c (+ (choose k (/ k 4)) (choose off 0)))
  ;(= w (+ (choose p (/ p 4)) (choose off 0)))
  ;(+ (* r w) c))

; Function for indexing into the matrix B.
;(procedure int (indexB [int off] [int k] [int j] [int m])
  ;(+ (* (+ k off) (/ m 4)) j))
  ;(: int r c w)
  ;(= r (+ (choose k (/ k 4)) (choose off 0)))
  ;(= c (+ (choose j (/ j 4)) (choose off 0)))
  ;(= w (+ (choose m (/ m 4)) (choose off 0)))
  ;(+ (* r w) c))

; Function for indexing into the matrix C. 
;(procedure int (indexC [int off] [int i] [int j] [int m])
  ;(+ (* (+ (/ i 4) off) (/ m 4)) j))
  ;(: int r c w)
  ;(= r (+ (choose i (/ i 4)) (choose off 0)))
  ;(= c (+ (choose j (/ j 4)) (choose off 0)))
  ;(= w (+ (choose m (/ m 4)) (choose off 0)))
  ;(+ (* r w) c))