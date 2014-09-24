#lang s-exp "../bv.rkt"

(provide p17 p17-stx)

(current-bitwidth 32)

; Turn-off the rightmost contiguous string of 1 bits.
(define (pp17 x)  
    (let* ([o1 (bvsub x 1)]
           [o2 (bvor x o1)]
           [o3 (bvadd o2 1)]
           [o4 (bvand o3 x)])
      o4))

(define (p17-spec x out) (= (pp17 x) out))

(define-fragment (p17 x) 
  #:ensures p17-spec
  #:library (bvlib [{const bvand bvor bvadd bvsub} 1]))

(define (turn-off-right-most-1s x)
  (local [(define (turn-off-1s current-bit mask bound)
            (if (or (= 0 (bvand x current-bit)) (= bound 0)) 
                (bvand x (bvnot mask)) 
                (turn-off-1s (<< current-bit 1) (bvor mask current-bit) (- bound 1))))
          (define (loop mask bound) 
            (cond [(= bound 0) 0]  
                  [(not (= 0 (bvand x mask))) (turn-off-1s mask mask bound)]
                  [else  (loop (<< mask 1) (- bound 1))]))]
    (loop 1 (current-bitwidth))))