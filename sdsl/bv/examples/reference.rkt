#lang s-exp "../bv.rkt"

(provide (all-defined-out))

; The 25 Hacker's Delight benchmarks from the following paper:
; Sumit Gulwani, Susmit Jha, Ashish Tiwari, and Ramarathnam Venkatesan. 2011. Synthesis of loop-free programs. In Proceedings of the 32nd ACM SIGPLAN Conference on Programming Language Design and Implementation (PLDI '11). 
;
; The first 20 benchmarks are also used in the SyGuS competition: http://www.sygus.org 


; Mask off the rightmost 1-bit.
(define (p1 x)  
  (let* ([o1 (bvsub x 1)]
         [o2 (bvand x o1)])
    o2))

; Test whether an unsigned integer is of the form 2^n-1.
(define (p2 x)  
  (let* ([o1 (bvadd x 1)]
         [o2 (bvand x o1)])
    o2))

; Isolate the right most 1-bit.
(define (p3 x)  
  (let* ([o1 (bvneg x)]
         [o2 (bvand x o1)])
    o2))

; Form a mask that identifies the rightmost 1-bit and trailing 0s.
(define (p4 x) 
  (let* ([o1 (bvsub x 1)]
         [o2 (bvxor x o1)])
    o2))

; Right propagate rightmost 1-bit.
(define (p5 x)  
  (let* ([o1 (bvsub x 1)]
         [o2 (bvor x o1)])
    o2))

; Turn on the right-most 0-bit in a word.
(define (p6 x) 
  (let* ([o1 (bvadd x 1)]
         [o2 (bvor x o1)])
    o2))

; Isolate the right most 0 bit of a given bitvector.
(define (p7 x)  
  (let* ([o1 (bvnot x)]
         [o2 (bvadd x 1)]
         [o3 (bvand o1 o2)])
    o3))

; Form a mask that identifies the trailing 0s.
(define (p8 x) 
  (let* ([o1 (bvsub x 1)]
         [o2 (bvnot x)]
         [o3 (bvand o1 o2)])
    o3))

; Absolute value function.
(define (p9 x) (abs x))

; Test if nlz(x) == nlz(y) where nlz is number of leading zeroes.
(define (p10 x y) 
  (let* ([o1 (bvand x y)]
         [o2 (bvxor x y)]
         [o3 (bvule o2 o1)])
    o3))

; Test if nlz(x) < nlz(y) where nlz is number of leading zeroes.
(define (p11 x y)  
  (let* ([o1 (bvnot y)]
         [o2 (bvand x o1)]
         [o3 (bvugt o2 y)])
    o3))

; Test if nlz(x) <= nlz(y) where nlz is number of leading zeroes.
(define (p12 x y)  
  (let* ([o1 (bvnot x)]
         [o2 (bvand y o1)]
         [o3 (bvule o2 x)])
    o3))

; Sign function.
(define (p13 x) (sgn x))

; Floor of average of two integers without over-flowing.
(define (p14 x y) 
  (let* ([o1 (bvand x y)]
         [o2 (bvxor x y)]
         [o3 (bvshr o2 1)]
         [o4 (bvadd o1 o3)])
    o4))

; Ceil of average of two integers without over-flowing.
(define (p15 x y)  
  (let* ([o1 (bvor x y)]
         [o2 (bvxor x y)]
         [o3 (bvshr o2 1)]
         [o4 (bvsub o1 o3)])
    o4))

; The max function.
(define (p16 x y) (if (bvge x y) x y))

; Turn-off the rightmost contiguous string of 1 bits.
(define (p17 x)  
  (let* ([o1 (bvsub x 1)]
         [o2 (bvor x o1)]
         [o3 (bvadd o2 1)]
         [o4 (bvand o3 x)])
    o4))

; Test whether an unsigned integer is of the form 2^n.
(define (p18 x)  
  (let* ([o1 (bvsub x 1)]
         [o2 (bvand o1 x)]
         [o3 (bvredor x)]
         [o4 (bvredor o2)]
         [o5 (bvnot o4)]
         [o6 (bvand o5 o3)])
    o6))  

; Exchanging 2 fields A and B of the same register 
; x where m is mask which identifies field B and k 
; is number of bits from end of A to start of B.
(define (p19 x m k)
  (let* ([o1 (bvshr x k)]
         [o2 (bvxor x o1)]
         [o3 (bvand o2 m)]
         [o4 (bvshl o3 k)]
         [o5 (bvxor o4 o3)]
         [o6 (bvxor o5 x)])
    o6))