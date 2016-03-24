#lang rosette

(require 
  (prefix-in $ (only-in rosette bv bveq bvslt bvsgt bvsle bvsge bvult bvugt bvule bvuge)))

(provide 
 bveq bvslt bvsgt bvsle bvsge bvult bvugt bvule bvuge
 bvnot bvor bvand bvxor bvshl bvlshr bvashr
 bvneg bvadd bvsub bvmul bvudiv bvsdiv bvurem bvsrem bvsmod
 bvredand bvredor bvconst BV bv bv*)

; Parameter that controls the type of bitvector operations.
(define BV (make-parameter (bitvector 4)))

(define (bvconst v) (lambda () (bv v)))

(define (bv v [t (BV)]) ($bv v t))

(define (bv* [t (BV)])
  (define-symbolic* b t)
  b)

(define-syntax-rule (bool->bv b) (if b (bv 1) (bv 0)))

(define (bvredor x)  (bveq (bveq x (bv 0)) (bv 0)))
(define (bvredand x) (bveq x (bv -1)))

(define-syntax-rule (define-comparators [id op] ...)
  (begin
    (define (id x y) (bool->bv (op x y))) ...))

(define-comparators 
  [bveq $bveq] 
  [bvslt $bvslt] [bvult $bvult]
  [bvsle $bvsle] [bvule $bvule]
  [bvsgt $bvsgt] [bvugt $bvugt]
  [bvsge $bvsge] [bvuge $bvuge])



