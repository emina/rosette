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