#lang racket
 
(require rosette/lib/util/roseunit)

(run-all-tests 
 "base/effects.rkt" 
 "base/type.rkt" 
 "base/term.rkt"
 "base/bool.rkt"
 "base/bitvector.rkt"
 "base/real.rkt"
 "base/num.rkt"
 "base/list.rkt"
 "base/equality.rkt"
 "base/merge.rkt"
 "base/vector.rkt"
 "base/finitize.rkt"
 "query/verify.rkt"
 "solver/z3.rkt"
 )

#|
(require rosette)
(term-cache)
(asserts)
(current-oracle)
(current-bitwidth)
(current-solver)
(current-solution)
|#