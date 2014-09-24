#lang s-exp "../bv.rkt"

(provide p25 p25-stx)

(current-bitwidth 32)

(define (pp25 x y)
  (let* ([o1  (bvand x #xffff)]
         [o2  (bvshr x 16)]
         [o3  (bvand y #xffff)]
         [o4  (bvshr y 16)]
         [o5  (bvmul o1 o3)]
         [o6  (bvmul o2 o3)]
         [o7  (bvmul o1 o4)]
         [o8  (bvmul o2 o4)]
         [o9  (bvshr o5 16)]
         [o10 (bvadd o6 o9)]
         [o11 (bvand o10 #xffff)]
         [o12 (bvshr o10 16)]
         [o13 (bvadd o7 o11)]
         [o14 (bvshr o13 16)]
         [o15 (bvadd o14 o12)]
         [o16 (bvadd o15 o8)])
    o16))

(define (p25-post x y out) (= (pp25 x y) out))

(define-values (c16 c#xffff)
  (values (thunk 16) (thunk #xffff)))

(define-fragment (p25 x y)
  #:min-bitwidth 32
  #:ensures p25-post
  #:library (bvlib [{c16 c#xffff} 1] 
                   [{bvand} 3] 
                   [{bvadd bvmul} 4] 
                   [{bvshr} 5]))



; Sanity check.
(for* ([i (in-range -16 16)]
       [j (in-range -16 16)])
   (p25-post i j (p25 i j)))


(define (mulmask x y)
  (let ([half-bw (/ (current-bitwidth) 2)])
    (>> (bitwise-and (* x y) (<< -1 half-bw)) half-bw)))
