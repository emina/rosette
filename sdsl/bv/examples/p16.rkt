#lang s-exp "../bv.rkt"

(provide p16 p16-stx)

(current-bitwidth 32)

; Compute max of two integers.
(define (max x y) (if (>= x y) x y))

(define (p16-post x y out) (= (max x y) out))

(define-fragment (p16 x y)
  #:ensures p16-post
  #:library (bvlib [{bvneg bvge bvand} 1] [{bvxor} 2]))

(define (pp16 x y) ; (bug in the paper: bvuge should be bvge)
  (let* ([o1 (bvxor x y)]
         [o2 (bvuge x y)]
         [o3 (bvneg o2)]
         [o4 (bvand o1 o2)]
         [o5 (bvxor o4 y)])
    o5))