#lang racket

(require racket/runtime-path "smt.rkt" "../common/server.rkt")

(provide cvc4%)

(define-runtime-path cvc4 (build-path ".." ".." ".." "bin" "cvc4"))

(define cvc4%
  (class* smt% (writable<%>) (inspect (make-inspector))

    (super-new [path cvc4] 
               [opts '("--lang=smt" "--output-lang=z3-str" "-m" "-q" "--strings-exp")])
    
    (define/public (custom-write port) (fprintf port "cvc4%"))
    (define/public (custom-display port) (custom-write port))))