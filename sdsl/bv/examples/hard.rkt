#lang rosette

(require "../bv.rkt")
(require "reference.rkt")
(provide (all-defined-out))

(BV (bitvector 32))

; Next higher unsigned number with the same number of 1 bits. ~ 600 sec.
(define-fragment (p20* x) 
  #:implements p20
  #:library (bvlib [{bv2 bvneg bvand bvadd bvxor bvlshr bvudiv bvor} 1]))

