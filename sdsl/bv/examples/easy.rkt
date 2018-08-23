#lang rosette

(require "../bv.rkt")
(require "reference.rkt")
(provide (all-defined-out))

(BV (bitvector 32))

; Mask off the rightmost 1-bit. < 1 sec.
(define-fragment (p1* x) 
  #:implements p1
  #:library (bvlib [{bv1 bvand bvsub} 1]))

; Test whether an unsigned integer is of the form 2^n-1. < 1 sec.
(define-fragment (p2* x) 
  #:implements p2
  #:library (bvlib [{bv1 bvand bvadd} 1]))

; Isolate the right most 1-bit. < 1 sec.
(define-fragment (p3* x) 
  #:implements p3
  #:library (bvlib [{bvand bvneg} 1]))

; Form a mask that identifies the rightmost 1-bit and trailing 0s. < 1 sec.
(define-fragment (p4* x) 
  #:implements p4
  #:library (bvlib [{bv1 bvsub bvxor} 1]))

; Right propagate rightmost 1-bit. < 1 sec.
(define-fragment (p5* x) 
  #:implements p5 
  #:library (bvlib [{bv1 bvsub bvor} 1]))

; Turn on the right-most 0-bit in a word. < 1 sec.
(define-fragment (p6* x) 
  #:implements p6
  #:library (bvlib [{bv1 bvor bvadd} 1]))

; Isolate the right most 0 bit of a given bitvector. < 1 sec.
(define-fragment (p7* x) 
  #:implements p7
  #:library (bvlib [{bvand bvadd bvnot bv1} 1]))

; Form a mask that identifies the trailing 0s. < 1 sec.
(define-fragment (p8* x) 
  #:implements p8
  #:library (bvlib [{bv1 bvsub bvand bvnot} 1]))

; Absolute value function. ~ 1 sec.
(define-fragment (p9* x) 
  #:implements p9
  #:library (bvlib [{bvsz bvsub bvxor bvashr} 1]))

; Test if nlz(x) == nlz(y) where nlz is number of leading zeroes. < 1 sec.
(define-fragment (p10* x y) 
  #:implements p10
  #:library (bvlib [{bvand bvxor bvule} 1]))
