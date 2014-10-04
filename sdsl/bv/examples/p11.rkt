#lang s-exp "../bv.rkt"

(provide p11 p11-stx)

(current-bitwidth 32)

; Test if nlz(x) < nlz(y) where nlz is number of leading zeroes.
(define (pp11 x y)  
    (let* ([o1 (bvnot y)]
           [o2 (bvand x o1)]
           [o3 (bvugt o2 y)])
      o3))

(define (p11-spec x y out) (= (pp11 x y) out))

(define-fragment (p11 x y) 
  #:ensures p11-spec
  #:library (bvlib [{bvnot bvand bvugt} 1]))

(define (nlz<? x y) 
  (local [(define bw (current-bitwidth))
          (define (loop mask bound) 
            (and (> bound 0)
                 (let ([x-bit (bvand x mask)]
                       [y-bit (bvand y mask)])
                   (and (= y-bit 0)
                        (or (not (= x-bit 0))
                            (loop (>> mask 1) (- bound 1)))))))]
    (loop (<< -1 (- bw 1)) bw)))