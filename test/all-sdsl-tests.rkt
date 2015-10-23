#lang racket

(define-syntax-rule (module@ id)
  (module+ id
    (require "../sdsl/websynth/test/all-tests.rkt")  ; WebSynth
    (require "../sdsl/bv/test/all-tests.rkt")        ; BV
    (require (submod "../sdsl/ifc/test.rkt" id))))   ; IFC tests

(module@ test) ; All tests
(module@ fast) ; Fast tests



