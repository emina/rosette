#lang s-exp "../bv.rkt"

(provide p23 p23-stx)

(current-bitwidth 32)

; Counting number of bits.
(define (pp23 x) 
  (let* ([o1  (bvshr x 1)]
         [o2  (bvand o1 #x55555555)]
         [o3  (bvsub x o2)]
         [o4  (bvand o3 #x33333333)]
         [o5  (bvshr o3 2)]
         [o6  (bvand o5 #x33333333)]
         [o7  (bvadd o4 o6)]
         [o8  (bvshr o7 4)]
         [o9  (bvadd o8 o7)]
         [o10 (bvand o9 #x0f0f0f0f)])
    o10))

(define (p23-post in out) (= (pp23 in) out))

(define-values (c1 c2 c4 c#x55555555 c#x33333333 c#x0f0f0f0f)
  (values (thunk 1) (thunk 2) (thunk 4) 
          (thunk #x55555555) (thunk #x33333333) (thunk #x0f0f0f0f)))

(define-fragment (p23 bv)
  #:ensures p23-post
  #:library (bvlib [{c#x55555555 c#x33333333 c#x0f0f0f0f c1 c2 c4} 1]
                   [{bvsub} 1] [{bvadd} 2] [{bvshr} 3] [{bvand} 4]))



