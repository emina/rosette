#lang racket

(define-syntax-rule (module@ id)
  (module+ id
    (require "memory.rkt")
    (require "operators.rkt")
    (require "reals.rkt")
    (require "typecheck.rkt")
    (require "work.rkt")
    (require (submod "../examples/matrixMultiply/synth/test.rkt" id))
    (require (submod "../examples/matrixMultiply/verify/test.rkt" id))
    (require (submod "../examples/sobelFilter/test.rkt" id))
    (require "../examples/fastWalshTransform/synth/test.rkt"))) 

(module@ test) ; All tests
(module@ fast) ; Fast tests



