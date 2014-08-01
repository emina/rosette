#lang racket

(require rosette/solver/kodkod/kodkod 
         (prefix-in solve/ "solve.rkt")
         (prefix-in solve+/ "solve+.rkt"))

(solve/run-tests-with (new kodkod%))
(solve+/run-tests-with (new kodkod-incremental%))