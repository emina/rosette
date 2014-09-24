#lang s-exp "../bv.rkt"

(provide p19 p19-stx)

(current-bitwidth 32) 

; Exchanging 2 fields A and B of the same register 
; x where m is mask which identifies field B and k 
; is number of bits from end of A to start of B.
(define (pp-19 x m k)
  (let* ([o1 (bvshr x k)]
         [o2 (bvxor x o1)]
         [o3 (bvand o2 m)]
         [o4 (bvshl o3 k)]
         [o5 (bvxor o4 o3)]
         [o6 (bvxor o5 x)])
    o6))

(define (p19-spec x m k out) (= (pp-19 x m k) out))

(define (p19-pre x m k) (&& (<= 0 k) (<= k (current-bitwidth))))
  
(define-fragment (p19 x m k) 
  #:requires p19-pre
  #:ensures p19-spec
  #:library (bvlib [{bvxor} 3] [{bvand bvshl bvshr} 1]))

