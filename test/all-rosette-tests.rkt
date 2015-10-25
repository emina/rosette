#lang racket
 
(require rosette/lib/util/roseunit)

(run-all-tests 
 "base/effects.rkt" 
 "base/type.rkt" 
 "base/term.rkt"
 "base/bool.rkt"
 "base/num.rkt"
 "base/list.rkt"
 "base/equality.rkt"
 "base/merge.rkt"
 "base/vector.rkt"
 "query/verify.rkt"
 "solver/kodkod.rkt"
 "solver/z3.rkt"
 "solver/bvsemantics.rkt"
 )

; (require rosette)(term-cache)(asserts)(current-oracle)(current-bitwidth)
