#lang s-exp "../bv.rkt"

(provide p12 p12-stx)

(current-bitwidth 32)

; Test if nlz(x) <= nlz(y) where nlz is number of leading zeroes.
(define (pc12 x y)  
    (let* ([o1 (bvnot x)]
           [o2 (bvand y o1)]
           [o3 (bvule o2 x)])
      o3))

(define (p12-spec x y out) (= (pc12 x y) out))

(define-fragment (p12 x y) 
  #:ensures p12-spec
  #:library (bvlib [{bvand bvnot bvule} 1]))

(define (nlz<=? x y)
  (local [(define bw (current-bitwidth))
          (define (loop mask bound) 
            (or (= bound 0)
                (let ([x-bit (bitwise-and x mask)]
                      [y-bit (bitwise-and y mask)])
                  (if (not (= x-bit y-bit))
                      (= y-bit 0)
                      (or (not (= x-bit 0))
                           (loop (>> mask 1) (- bound 1)))))))]
    (loop (<< -1 (- bw 1)) bw)))

(define (pp12 x y) ; (bug in the paper: x and y transposed in the body)
    (let* ([o1 (bvnot y)]
           [o2 (bvand x o1)]
           [o3 (bvule o2 y)])
      o3))