#lang s-exp "../bv.rkt"

(require "consts.rkt")

(provide p13 p13-stx)

(current-bitwidth 32)

; Sign function.
(define (p13-spec x out) (= (sgn x) out))

(define-fragment (p13 x) 
  #:ensures p13-spec
  #:library (bvlib [{const-max-shift bvor} 1] [{bvneg bvshr} 2]))

(define (pp13 x) ; (bug in the paper:  at least 2 bvneg instructions needed but only 1 specified)
  (let* ([o1 (bvshr x 31)]
         [o2 (bvneg x)]
         [o3 (bvshr o2 31)]
         [o4 (bvor o1 o3)])
     o4))

(define (show-bug)
  (define-symbolic x number?)
  (verify (assert (= (pp13 x) (sgn x))))
  (define cex ((current-solution) x))
  (printf "(pp13 ~a) = ~a, expected ~a\n" cex (pp13 cex) (sgn cex)))