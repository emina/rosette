#lang s-exp "../bv.rkt"

(provide p7 p7-stx)

(current-bitwidth 32)

; Isolate the right most 0 bit of a given bitvector.
(define (pp7 x) ; from the paper
    (let* ([o1 (bvnot x)]
           [o2 (bvadd x 1)]
           [o3 (bvand o1 o2)])
      o3))

(define (p7-spec x out) (= (pp7 x) out))

(define-fragment (p7 x) 
  #:ensures p7-spec
  #:library (bvlib [{bvand bvadd bvnot const} 1]))

(define (isolate-right-most-0 x)
  (local [(define (loop mask bound) 
            (cond [(= bound 0) 0] 
                  [(not (= 0 (bitwise-and x mask))) (loop (<< mask 1) (- bound 1))]                                
                  [else mask]))]
    (loop 1 (current-bitwidth))))