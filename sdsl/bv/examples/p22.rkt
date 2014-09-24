#lang s-exp "../bv.rkt"

(provide p22 p22-stx)

(current-bitwidth 32)

; Compute parity.
(define (pp22 x) 
  (let* ([o1 (bvshr x 1)]
         [o2 (bvxor o1 x)]
         [o3 (bvshr o2 2)]
         [o4 (bvxor o2 o3)]
         [o5 (bvand o4 #x11111111)]
         [o6 (bvmul o5 #x11111111)]
         [o7 (bvshr o6 28)]
         [o8 (bvand o7 1)]) 
    o8))

(define (p22-post x out) (= (pp22 x) out))

(define-values (c1 c2 c28 cx11111111) 
  (values (thunk 1) (thunk 2) (thunk 28) (thunk #x11111111)))

(define-fragment (p22 x) 
  #:min-bitwidth 32
  #:ensures p22-post
  #:library (bvlib [{c1 c2 c28 cx11111111 bvmul} 1] [{bvxor bvand} 2] [{bvshr} 3]))  

