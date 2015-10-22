#lang s-exp "../bv.rkt"

(require "reference.rkt" "consts.rkt")
(provide (all-defined-out))

(current-bitwidth 32)

; Mask off the rightmost 1-bit. < 1 sec.
(define-fragment (p1* x) 
  #:implements p1
  #:library (bvlib [{const bvand bvsub} 1]))

; Test whether an unsigned integer is of the form 2^n-1. < 1 sec.
(define-fragment (p2* x) 
  #:implements p2
  #:library (bvlib [{const bvand bvadd} 1]))

; Isolate the right most 1-bit. < 1 sec.
(define-fragment (p3* x) 
  #:implements p3
  #:library (bvlib [{bvand bvneg} 1]))

; Form a mask that identifies the rightmost 1-bit and trailing 0s. < 1 sec.
(define-fragment (p4* x) 
  #:implements p4
  #:library (bvlib [{const bvsub bvxor} 1]))

; Right propagate rightmost 1-bit. < 1 sec.
(define-fragment (p5* x) 
  #:implements p5 
  #:library (bvlib [{const bvsub bvor} 1]))

; Turn on the right-most 0-bit in a word. < 1 sec.
(define-fragment (p6* x) 
  #:implements p6
  #:library (bvlib [{const bvor bvadd} 1]))

; Isolate the right most 0 bit of a given bitvector. < 1 sec.
(define-fragment (p7* x) 
  #:implements p7
  #:library (bvlib [{bvand bvadd bvnot const} 1]))

; Form a mask that identifies the trailing 0s. < 1 sec.
(define-fragment (p8* x) 
  #:implements p8
  #:library (bvlib [{const bvsub bvand bvnot} 1]))

; Absolute value function. ~ 1 sec.
(define-fragment (p9* x) 
  #:implements p9
  #:library (bvlib [{const-max-shift bvsub bvxor bvshr} 1]))

; Test if nlz(x) == nlz(y) where nlz is number of leading zeroes. < 1 sec.
(define-fragment (p10* x y) 
  #:implements p10
  #:library (bvlib [{bvand bvxor bvule} 1]))