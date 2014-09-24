#lang s-exp "../bv.rkt"

(provide p10 p10-stx)

(current-bitwidth 32)

; Test if nlz(x) == nlz(y) where nlz is number of leading zeroes.
(define (pp10 x y) 
    (let* ([o1 (bvand x y)]
           [o2 (bvxor x y)]
           [o3 (bvule o2 o1)])
      o3))

(define (p10-spec x y out) (= (pp10 x y) out))

(define-fragment (p10 x y) 
  #:ensures p10-spec
  #:library (bvlib [{bvand bvxor bvule} 1]))

(define (nlz=? x y) 
  (local [(define bw (current-bitwidth))
          (define (loop mask bound) 
            (or (= bound 0)
                (let ([x-bit (bvand x mask)]
                      [y-bit (bvand y mask)])
                  (and (= x-bit y-bit)
                       (or (not (= x-bit 0))
                           (loop (>> mask 1) (- bound 1)))))))]
    (loop (<< -1 (- bw 1)) bw)))