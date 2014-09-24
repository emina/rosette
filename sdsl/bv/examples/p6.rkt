#lang s-exp "../bv.rkt"

(provide p6 p6-stx)

(current-bitwidth 32)

; Turn on the right-most 0-bit in a word.
(define (pp6 x) 
  (let* ([o1 (bvadd x 1)]
         [o2 (bvor x o1)])
    o2))

(define (p6-spec x out) (= (pp6 x) out))

(define-fragment (p6 x) 
  #:ensures p6-spec
  #:library (bvlib [{const bvor bvadd} 1]))

(define (flip-right-most-0 x)
  (local [(define (loop mask bound) 
            (cond [(= bound 0) -1]
                  [(= 0 (bvand x mask)) (bvor x mask)]  
                  [else (loop (<< mask 1) (- bound 1))]))]
    (loop 1 (current-bitwidth))))