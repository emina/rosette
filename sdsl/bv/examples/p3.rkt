#lang s-exp "../bv.rkt"

(provide p3 p3-stx)

(current-bitwidth 32)

; Isolate the right most 1-bit.
(define (pp3 x)  
  (let* ([o1 (bvneg x)]
         [o2 (bvand x o1)])
    o2))

(define (p3-spec x out) (= (pp3 x) out))

(define-fragment (p3 x) 
  #:ensures p3-spec
  #:library (bvlib [{bvand bvneg} 1]))

(define (isolate-right-most-1 x)
  (local [(define (loop mask bound) 
            (cond [(= bound 0) 0]
                  [(= (bvand x mask) 0) (loop (<< mask 1) (- bound 1))]
                  [else mask]))]
    (loop 1 (current-bitwidth))))

