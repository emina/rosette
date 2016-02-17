#lang racket

(require rosette/lib/util/roseunit)

(test-groups [test fast]
 "memory.rkt"
 "operators.rkt"
 "reals.rkt"
 "typecheck.rkt"
 "work.rkt"
 ;(submod "../examples/matrixMultiply/synth/test.rkt")
 ;(submod "../examples/matrixMultiply/verify/test.rkt")
 ;(submod "../examples/sobelFilter/test.rkt")
 (submod "../examples/fastWalshTransform/synth/test.rkt")
 (submod "../examples/fastWalshTransform/verify/test.rkt")
)



