#lang s-exp "../bv.rkt"

(provide p4 p4-stx)

(current-bitwidth 32)

; Form a mask that identifies the rightmost 1-bit and trailing 0s.
(define (pp4 x) 
    (let* ([o1 (bvsub x 1)]
           [o2 (bvxor x o1)])
      o2))

(define (p4-spec x out) (= (pp4 x) out))

(define-fragment (p4 x) 
  #:ensures p4-spec
  #:library (bvlib [{const bvsub bvxor} 1]))

(define (mask-right-most-1 x)
  (local [(define (loop mask bound) 
            (cond [(= bound 0) -1]
                  [(not (= 0 (bvand x mask))) (bvnot (<< -1 (integer-length mask)))]                              
                  [else (loop (<< mask 1) (- bound 1))]))]
    (loop 1 (current-bitwidth))))