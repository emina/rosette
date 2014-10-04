#lang racket

(require rosette/solver/smt/cvc4
         (prefix-in solve/ "solve.rkt")
         (prefix-in solve+/ "solve+.rkt"))

(solve/run-tests-with (new cvc4%))
(solve+/run-tests-with (new cvc4%))