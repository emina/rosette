#lang racket

(define-syntax-rule (module@ id)
  (module+ id
    (require "../sdsl/websynth/test/all-tests.rkt")            ; WebSynth
    (require "../sdsl/bv/test/all-tests.rkt")                  ; BV
    (require (submod "../sdsl/ifc/test.rkt" id))               ; IFC tests
    (require (submod "../sdsl/synthcl/test/all-tests.rkt" id)) ; SynthCL tests
    ))   

(module@ test) ; All tests
(module@ fast) ; Fast tests



