#lang s-exp "../bv.rkt"

(provide p20 p20-stx)

(current-bitwidth 32) 

; Next higher unsigned number with the same number of 1 bits.
(define (pp20 x)
  (let* ([o1 (bvneg x)]
         [o2 (bvand x o1)]
         [o3 (bvadd x o2)]
         [o4 (bvxor x o2)]
         [o5 (bvshr o4 2)]
         [o6 (bvdiv o5 o2)]
         [o7 (bvor o6 o3)])
    o7))

(define (p20-pre x) (! (= x 0)))

(define (p20-spec x out) (= (pp20 x) out))

(define c2 (thunk 2))

(define-fragment (p20 x) 
  #:requires p20-pre
  #:ensures p20-spec
  #:library (bvlib [{bvneg bvand bvadd bvxor bvshr c2 bvdiv bvor} 1]))

