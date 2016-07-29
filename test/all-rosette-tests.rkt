#lang racket
 
(require rosette/lib/roseunit)

(error-print-width 4)

(run-all-tests 
 "base/effects.rkt" 
 "base/type.rkt" 
 "base/term.rkt"
 "base/bool.rkt"
 "base/bitvector.rkt"
 "base/real.rkt"
 "base/equality.rkt"
 "base/merge.rkt"
 "base/uninterpreted.rkt"
 "base/quantified.rkt"
 "base/finitize.rkt"
 "base/distinct.rkt"
 "base/list.rkt"
 "base/vector.rkt"
 "base/generics.rkt"
 "query/solve.rkt"
 "query/verify.rkt"
 "query/synthesize.rkt"
 "query/debug.rkt"
 "query/solve+.rkt"
 "query/optimize.rkt"
 "query/synthax.rkt"
 "query/push-pop.rkt"
 )

#|
(require rosette)
(term-cache)
(asserts)
(current-oracle)
(current-bitwidth)
(current-solver)
|#
