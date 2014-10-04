#lang racket

(require rosette/solver/smt/z3 
         (prefix-in solve/ "solve.rkt")
         (prefix-in solve+/ "solve+.rkt"))

(solve/run-tests-with (new z3%))
(solve+/run-tests-with (new z3%))