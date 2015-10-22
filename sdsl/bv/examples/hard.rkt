#lang s-exp "../bv.rkt"

(require "reference.rkt" "consts.rkt")
(provide (all-defined-out))

(current-bitwidth 32)

(define-const 2 4 8 16 28 "#x11111111" "#x55555555" "#x33333333" "#x0f0f0f0f" "#xffff")

; Next higher unsigned number with the same number of 1 bits. ~ 830 sec.
(define-fragment (p20* x) 
  #:requires (lambda (x) (! (= x 0)))
  #:implements p20
  #:library (bvlib [{bvneg bvand bvadd bvxor bvshr const2 bvdiv bvor} 1]))

; Cycling through 3 values a, b, c. Timeout. > 1000 sec.
(define-fragment (p21* x a b c) 
  #:requires (lambda (x a b c) (|| (= x a) (= x b) (= x c)))
  #:implements p21
  #:library (bvlib [{bvxor} 4] [{bvand bvneg bveq} 2]))

; Compute parity. Timeout. > 1000 sec.
(define-fragment (p22* x) 
  #:min-bitwidth 32
  #:implements p22
  #:library (bvlib [{const1 const2 const28 const#x11111111 bvmul} 1] 
                   [{bvxor bvand} 2] 
                   [{bvshr} 3]))  

; Counting number of bits. Timeout. > 1000 sec.
(define-fragment (p23* x)
  #:implements p23
  #:library (bvlib [{const#x55555555 const#x33333333 const#x0f0f0f0f const1 const2 const4} 1]
                   [{bvsub} 1] [{bvadd} 2] [{bvshr} 3] [{bvand} 4]))

; Round up to the next higher power of 2. Timeout. > 1000 sec.
(define-fragment (p24* x)
  #:min-bitwidth 16
  #:implements p24
  #:library (bvlib [{bvsub bvadd} 1]
                   [{const1 const2 const4 const8 const16} 1]
                   [{bvor} 5] 
                   [{bvshr} 5]))

; Compute higher order half of product of x and y.  Timeout. > 1000 sec. 
(define-fragment (p25* x y)
  #:min-bitwidth 32
  #:implements p25
  #:library (bvlib [{const16 const#xffff} 1] 
                   [{bvand} 3] 
                   [{bvadd bvmul} 4] 
                   [{bvshr} 5]))

