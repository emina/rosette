#lang rosette

(require "../bv.rkt")
(provide (all-defined-out))

; The 25 Hacker's Delight benchmarks from the following paper:
; Sumit Gulwani, Susmit Jha, Ashish Tiwari, and Ramarathnam Venkatesan. 
; 2011. Synthesis of loop-free programs. In Proceedings of the 32nd ACM 
; SIGPLAN Conference on Programming Language Design and Implementation (PLDI '11). 
;
; The first 20 benchmarks are also used in the SyGuS competition: http://www.sygus.org 

; Constants.
(define bv1  (thunk (bv 1)))
(define bv2  (thunk (bv 2)))
(define bvsz (thunk (bv (sub1 (bitvector-size (BV))))))

; Mask off the rightmost 1-bit.
(define (p1 x)  
  (let* ([o1 (bvsub x (bv 1))]
         [o2 (bvand x o1)])
    o2))

; Test whether an unsigned integer is of the form 2^n-1.
(define (p2 x)  
  (let* ([o1 (bvadd x (bv 1))]
         [o2 (bvand x o1)])
    o2))

; Isolate the right most 1-bit.
(define (p3 x)  
  (let* ([o1 (bvneg x)]
         [o2 (bvand x o1)])
    o2))

; Form a mask that identifies the rightmost 1-bit and trailing 0s.
(define (p4 x) 
  (let* ([o1 (bvsub x (bv 1))]
         [o2 (bvxor x o1)])
    o2))

; Right propagate rightmost 1-bit.
(define (p5 x)  
  (let* ([o1 (bvsub x (bv 1))]
         [o2 (bvor x o1)])
    o2))

; Turn on the right-most 0-bit in a word.
(define (p6 x) 
  (let* ([o1 (bvadd x (bv 1))]
         [o2 (bvor x o1)])
    o2))

; Isolate the right most 0 bit of a given bitvector.
(define (p7 x)  
  (let* ([o1 (bvnot x)]
         [o2 (bvadd x (bv 1))]
         [o3 (bvand o1 o2)])
    o3))

; Form a mask that identifies the trailing 0s.
(define (p8 x) 
  (let* ([o1 (bvsub x (bv 1))]
         [o2 (bvnot x)]
         [o3 (bvand o1 o2)])
    o3))

; Absolute value function.
(define (p9 x) 
  (let* ([o1 (bvashr x (bv 31))]
         [o2 (bvxor x o1)]
         [o3 (bvsub o2 o1)])
    o3))

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
(define (p13 x) 
  (let* ([o1 (bvneg x)]
         [o2 (bvlshr o1 (bv 31))]
         [o3 (bvashr x (bv 31))]
         [o4 (bvor o2 o3)])
    o4))

; Floor of average of two integers without over-flowing.
(define (p14 x y) 
  (let* ([o1 (bvand x y)]
         [o2 (bvxor x y)]
         [o3 (bvlshr o2 (bv 1))]
         [o4 (bvadd o1 o3)])
    o4))

; Ceil of average of two integers without over-flowing.
(define (p15 x y)  
  (let* ([o1 (bvor x y)]
         [o2 (bvxor x y)]
         [o3 (bvlshr o2 (bv 1))]
         [o4 (bvsub o1 o3)])
    o4))

; The max function.
(define (p16 x y) (if (equal? (bv 1) (bvsge x y)) x y))

; Turn-off the rightmost contiguous string of 1 bits.
(define (p17 x)  
  (let* ([o1 (bvsub x (bv 1))]
         [o2 (bvor x o1)]
         [o3 (bvadd o2 (bv 1))]
         [o4 (bvand o3 x)])
    o4))

; Test whether an unsigned integer is of the form 2^n.
(define (p18 x)  
  (let* ([o1 (bvsub x (bv 1))]
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
  (let* ([o1 (bvlshr x k)]
         [o2 (bvxor x o1)]
         [o3 (bvand o2 m)]
         [o4 (bvshl o3 k)]
         [o5 (bvxor o4 o3)]
         [o6 (bvxor o5 x)])
    o6))

; Next higher unsigned number with the same number of 1 bits.
(define (p20 x)
  (let* ([o1 (bvneg x)]
         [o2 (bvand x o1)]
         [o3 (bvadd x o2)]
         [o4 (bvxor x o2)]
         [o5 (bvlshr o4 (bv 2))]
         [o6 (bvudiv o5 o2)]
         [o7 (bvor o6 o3)])
    o7))

; Cycling through 3 values a, b, c.
(define (p21 x a b c)  
  (let* ([o1 (bveq x c)]
         [o2 (bvneg o1)]
         [o3 (bvxor a c)]
         [o4 (bveq x a)]
         [o5 (bvneg o4)]
         [o6 (bvxor b c)]
         [o7 (bvand o2 o3)]
         [o8 (bvand o5 o6)]
         [o9 (bvxor o7 o8)]
         [o10 (bvxor o9 c)])
    o10))

; Compute parity.
(define (p22 x) 
  (let* ([o1 (bvlshr x (bv 1))]
         [o2 (bvxor o1 x)]
         [o3 (bvlshr o2 (bv 2))]
         [o4 (bvxor o2 o3)]
         [o5 (bvand o4 (bv #x11111111))]
         [o6 (bvmul o5 (bv #x11111111))]
         [o7 (bvlshr o6 (bv 28))]
         [o8 (bvand o7 (bv 1))]) 
    o8))

; Counting number of bits.
(define (p23 x) 
  (let* ([o1  (bvlshr x (bv 1))]
         [o2  (bvand o1 (bv #x55555555))]
         [o3  (bvsub x o2)]
         [o4  (bvand o3 (bv #x33333333))]
         [o5  (bvlshr o3 (bv 2))]
         [o6  (bvand o5 (bv #x33333333))]
         [o7  (bvadd o4 o6)]
         [o8  (bvlshr o7 (bv 4))]
         [o9  (bvadd o8 o7)]
         [o10 (bvand o9 (bv #x0f0f0f0f))])
    o10))

; Round up to the next higher power of 2.
(define (p24 x) 
  (let* ([o1  (bvsub x (bv 1))]
         [o2  (bvlshr o1 (bv 1))]
         [o3  (bvor o1 o2)]
         [o4  (bvlshr o3 (bv 2))]
         [o5  (bvor o3 o4)]
         [o6  (bvlshr o5 (bv 4))]
         [o7  (bvor o5 o6)]
         [o8  (bvlshr o7 (bv 8))]
         [o9  (bvor o7 o8)]
         [o10 (bvlshr o9 (bv 16))]
         [o11 (bvor o9 o10)]
         [o12 (bvadd o11 (bv 1))])
    o12))

; Compute higher order half of product of x and y.
(define (p25 x y)
  (let* ([o1  (bvand x (bv #xffff))]
         [o2  (bvlshr x (bv 16))]
         [o3  (bvand y (bv #xffff))]
         [o4  (bvlshr y (bv 16))]
         [o5  (bvmul o1 o3)]
         [o6  (bvmul o2 o3)]
         [o7  (bvmul o1 o4)]
         [o8  (bvmul o2 o4)]
         [o9  (bvlshr o5 (bv 16))]
         [o10 (bvadd o6 o9)]
         [o11 (bvand o10 (bv #xffff))]
         [o12 (bvlshr o10 (bv 16))]
         [o13 (bvadd o7 o11)]
         [o14 (bvlshr o13 (bv 16))]
         [o15 (bvadd o14 o12)]
         [o16 (bvadd o15 o8)])
    o16))
