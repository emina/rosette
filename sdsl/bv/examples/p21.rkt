#lang s-exp "../bv.rkt"

(provide p21 p21-stx)

(current-bitwidth 32)

; Cycling through 3 values a, b, c.
(define (pp21 x a b c)  
  (let* ([o1 (bveq x c)]
         [o2 (bvneg o1)]
         [o3 (bvxor a c)]
         [o4 (bveq x a)]
         [o5 (bvneg o4)]
         [o6 (bvxor b c)]
         [o7 (bvand o2 o3)]
         [o8 (bvand o5 o6)]
         [o9 (bvxor o7 o8)]
         [o10 (bvxor o9 c)])
    o10))

(define (p21-pre x a b c)
  (|| (= x a) (= x b) (= x c)))

(define (p21-post x a b c out) 
  (= (pp21 x a b c) out))

(define-fragment (p21 x a b c) 
  #:requires p21-pre
  #:ensures p21-post
  #:library (bvlib [{bvxor} 4] [{bvand bvneg bveq} 2]))

