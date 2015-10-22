#lang s-exp "../bv.rkt"

(require "reference.rkt" "consts.rkt")
(provide (all-defined-out))

(current-bitwidth 32)

; Test if nlz(x) < nlz(y) where nlz is number of leading zeroes. < 1 sec.
(define-fragment (p11* x y) 
  #:implements p11
  #:library (bvlib [{bvnot bvand bvugt} 1]))

; Test if nlz(x) <= nlz(y) where nlz is number of leading zeroes. < 1 sec.
(define-fragment (p12* x y) 
  #:implements p12
  #:library (bvlib [{bvand bvnot bvule} 1]))

; Sign function. Bug in the PLDI'11 paper: at least 2 bvneg instructions 
; needed but only 1 specified. ~ 1.2 sec.
(define-fragment (p13* x) 
  #:implements p13
  #:library (bvlib [{const-max-shift bvor} 1] [{bvneg bvshr} 2]))

; Floor of average of two integers without over-flowing. < 1 sec.
(define-fragment (p14* x y) 
  #:implements p14
  #:library (bvlib [{const bvand bvshr bvxor bvadd} 1]))

; Ceil of average of two integers without over-flowing. ~ 1.3 sec.
(define-fragment (p15* x y) 
  #:implements p15
  #:library (bvlib [{const bvor bvshr bvxor bvsub} 1]))

; Compute max of two integers. Bug in the PLDI'11 paper: bvuge should be bvge.  
; ~ 1.3 sec.
(define-fragment (p16* x y)
  #:implements p16
  #:library (bvlib [{bvneg bvge bvand} 1] [{bvxor} 2]))

; Turn-off the rightmost contiguous string of 1 bits. ~ 1.8 sec.
(define-fragment (p17* x) 
  #:implements p17
  #:library (bvlib [{const bvand bvor bvadd bvsub} 1]))

; Test whether an unsigned integer is of the form 2^n. ~ 1.6 sec.
(define-fragment (p18* x) 
  #:implements p18
  #:library (bvlib [{const bvsub bvnot} 1] [{bvredor bvand} 2]))

; Exchanging 2 fields A and B of the same register 
; x where m is mask which identifies field B and k 
; is number of bits from end of A to start of B. ~ 3.5 sec.
(define-fragment (p19* x m k) 
  #:requires (lambda (x m k) (&& (<= 0 k) (<= k (current-bitwidth))))
  #:implements p19
  #:library (bvlib [{bvxor} 3] [{bvand bvshl bvshr} 1]))

