#lang s-exp "../bv.rkt"

(provide p24 p24-stx)

(current-bitwidth 32)

; Round up to the next higher power of 2.
(define (pp24 x) 
  (let* ([o1  (bvsub x 1)]
         [o2  (bvshr o1 1)]
         [o3  (bvor o1 o2)]
         [o4  (bvshr o3 2)]
         [o5  (bvor o3 o4)]
         [o6  (bvshr o5 4)]
         [o7  (bvor o5 o6)]
         [o8  (bvshr o7 8)]
         [o9  (bvor o7 o8)]
         [o10 (bvshr o9 16)]
         [o11 (bvor o9 o10)]
         [o12 (bvadd o11 1)])
    o12))

(define (p24-post x out) (= (pp24 x) out))

(define-values (c1 c2 c4 c8 c16)
  (values (thunk 1) (thunk 2) (thunk 4) (thunk 8) (thunk 16)))

(define-fragment (p24 x)
  #:min-bitwidth 16
  #:ensures p24-post
  #:library (bvlib [{bvsub bvadd} 1]
                   [{c1 c2 c4 c8 c16} 1]
                   [{bvor} 5] 
                   [{bvshr} 5]))

