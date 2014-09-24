#lang s-exp "../bv.rkt"

(provide p18 p18-stx)

(current-bitwidth 32) 

; Test whether an unsigned integer is of the form 2^n.
(define (pp18 x)  
    (let* ([o1 (bvsub x 1)]
           [o2 (bvand o1 x)]
           [o3 (bvredor x)]
           [o4 (bvredor o2)]
           [o5 (bvnot o4)]
           [o6 (bvand o5 o3)])
      o6))  

(define (p18-spec x out) (= (pp18 x) out))

(define-fragment (p18 x) 
  #:ensures p18-spec
  #:library (bvlib [{const bvsub bvnot} 1] [{bvredor bvand} 2]))

(define (2^n? x)
  (if (= x (- (<< 1 (- (current-bitwidth) 1))))
      1
      (let loop ([n (- (current-bitwidth) 2)])
        (cond [(< n 0) 0]
              [(= x (<< 1 n)) 1]
              [else (loop (- n 1))]))))  