#lang s-exp "../bv.rkt"

(provide p8 p8-stx)

(current-bitwidth 32)

; Form a mask that identifies the trailing 0s.
(define (pp8 x) 
    (let* ([o1 (bvsub x 1)]
           [o2 (bvnot x)]
           [o3 (bvand o1 o2)])
      o3))

(define (p8-spec x out) (= (pp8 x) out))

(define-fragment (p8 x) 
  #:ensures p8-spec
  #:library (bvlib [{const bvsub bvand bvnot} 1]))

(define (mask-trailing-0s x)
  (local [(define (loop mask bound) 
            (cond [(= bound 0) -1]
                  [(not (= 0 (bvand x mask))) (bvnot (<< -1 (- (integer-length mask) 1)))]                               
                  [else (loop (<< mask 1) (- bound 1))]))]
    (loop 1 (current-bitwidth))))