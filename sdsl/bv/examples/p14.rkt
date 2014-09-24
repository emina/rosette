#lang s-exp "../bv.rkt"

(provide p14 p14-stx)

(current-bitwidth 32)

; Floor of average of two integers without over-flowing.
(define (pp14 x y) 
  (let* ([o1 (bvand x y)]
         [o2 (bvxor x y)]
         [o3 (bvshr o2 1)]
         [o4 (bvadd o1 o3)])
     o4))
        
(define (p14-spec x y out) (= (pp14 x y) out))

(define-fragment (p14 x y) 
  #:ensures p14-spec
  #:library (bvlib [{const bvand bvshr bvxor bvadd} 1]))

(define (floor-of-ave x y)
  (+ (>> x 1) (>> y 1) (bvand (bvand 1 x) (bvand 1 y))))