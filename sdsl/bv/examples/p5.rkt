#lang s-exp "../bv.rkt"

(provide p5 p5-stx)

(current-bitwidth 32)

; Right propagate rightmost 1-bit.
(define (pp5 x)  
  (let* ([o1 (bvsub x 1)]
         [o2 (bvor x o1)])
    o2))

(define (p5-spec x out) (= (pp5 x) out))

(define-fragment (p5 x) 
  #:ensures p5-spec
  #:library (bvlib [{const bvsub bvor} 1]))

(define (propagate-right-most-1 x)
  (local [(define (loop mask bound) 
            (cond [(= bound 0) -1]
                  [(= 0 (bvand x mask)) (loop (<< mask 1) (- bound 1))]
                  [else (bvor x (bvnot (<< -1 (integer-length mask))))]))]
    (loop 1 (current-bitwidth))))