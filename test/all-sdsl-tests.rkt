#lang racket

(require rosette/lib/util/roseunit)

(test-groups [test fast] 
 ;"../sdsl/websynth/test/all-tests.rkt"         ; WebSynth
 ;"../sdsl/bv/test/all-tests.rkt"               ; BV
 ;(submod "../sdsl/ifc/test.rkt")               ; IFC tests
 ;(submod "../sdsl/synthcl/test/all-tests.rkt") ; SynthCL tests
)   





