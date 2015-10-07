#lang racket

(require "stats/stats.rkt")
(require rosette/config/log rosette/query/state rosette/solver/solution)
(require (only-in rosette oracle current-oracle clear-asserts unsafe-clear-terms!))
(current-log-handler (log-handler #:info none/c))

(define (run-test file)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-solution (empty-solution)]
                 [current-oracle (oracle)])
    (eval `(require ,file))
    (clear-asserts)
    (unsafe-clear-terms!)))

(define (base)
  (run-test "base/effects.rkt")
  (run-test "base/term.rkt")
  (run-test "base/bool.rkt")
  (run-test "base/num.rkt")
  (run-test "base/list.rkt")
  (run-test "base/equality.rkt")
  (run-test "base/merge.rkt"))

(define (forms)
  (run-test "forms/verify.rkt"))

(define (kodkod)
 (run-test "solver/kodkod.rkt"))

(define (z3)
  (run-test "solver/z3.rkt"))

(define (bv-semantics)
  (run-test "solver/bvsemantics.rkt"))

(run/time/log (base) "stats/all-tests.txt")
(run/time/log (forms) "stats/all-tests.txt")
(run/time/log (kodkod)  "stats/all-tests.txt")
(run/time/log (z3)  "stats/all-tests.txt")
(run/time/log (bv-semantics)  "stats/all-tests.txt")