#lang s-exp "../bv.rkt"

(provide p15 p15-stx)

(current-bitwidth 32)

; Ceil of average of two integers without over-flowing.
(define (pp15 x y) ; from the paper
  (let* ([o1 (bvor x y)]
         [o2 (bvxor x y)]
         [o3 (bvshr o2 1)]
         [o4 (bvsub o1 o3)])
     o4))
        
(define (p15-spec x y out) (= (pp15 x y) out))

(define-fragment (p15 x y) 
  #:ensures p15-spec
  #:library (bvlib [{const bvor bvshr bvxor bvsub} 1])) ; alternatively: (bvlib [{const bvneg bvor bvshr bvxor bvadd} 1])

(define (ceil-of-ave x y)
  (+ (>> x 1) (>> y 1) (bvor (bvand 1 x) (bvand 1 y))))