#lang s-exp "../bv.rkt"

(require "consts.rkt")

(provide p9 p9-stx)

(current-bitwidth 32)

; Absolute value function.
(define (p9-spec x out) (= (abs x) out))

(define-fragment (p9 x) 
  #:ensures p9-spec
  #:library (bvlib [{const-max-shift bvsub bvxor bvshr} 1]))

(define (pp9 x)  
    (let* ([o1 (bvshr x 31)]
           [o2 (bvxor x o1)]
           [o3 (bvsub o2 o1)])
      o3))