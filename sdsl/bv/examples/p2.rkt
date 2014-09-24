#lang s-exp "../bv.rkt"

(provide p2 p2-stx)

(current-bitwidth 32)

; Test whether an unsigned integer is of the form 2^n-1.
(define (pp2 x)  
    (let* ([o1 (bvadd x 1)]
           [o2 (bvand x o1)])
      o2))

(define (p2-spec x out) (= (pp2 x) out))

(define-fragment (p2 x) 
  #:ensures p2-spec
  #:library (bvlib [{const bvand bvadd} 1]))

(define (2^n-1? x)
  (cond [(< x -1) 0]
        [(= x -1) 1]
        [else (let loop ([n 0])
                (cond [(= n (current-bitwidth)) 0]
                      [(= x (- (<< 1 n) 1)) 1]
                      [else (loop (+ n 1))]))]))