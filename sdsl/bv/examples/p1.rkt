#lang s-exp "../bv.rkt"

(provide p1 p1-stx)

(current-bitwidth 32)

; Mask off the rightmost 1-bit.
(define (pp1 x)  
    (let* ([o1 (bvsub x 1)]
           [o2 (bvand x o1)])
      o2))

(define (p1-spec x out) (= (pp1 x) out))

(define-fragment (p1 x) 
  #:ensures p1-spec
  #:library (bvlib [{const bvand bvsub} 1]))

; Mask off the right most bit of a given bitvector.
(define (mask-off-right-most-1 x)
  (local [(define (loop mask bound) 
            (cond [(= bound 0) 0]  
                  [(not (= 0 (bvand x mask))) (bvand x (bvnot mask))]
                  [else  (loop (<< mask 1) (- bound 1))]))]
    (loop 1 (current-bitwidth))))