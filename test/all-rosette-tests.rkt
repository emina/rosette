#lang racket
 
(require (only-in rosette solver-features current-solver) "base/solver.rkt"
         rosette/lib/roseunit
         rosette/solver/smt/z3 rosette/solver/smt/cvc4 rosette/solver/smt/boolector)

(error-print-width 4)


; Require all the modules with tests. No tests will actually run,
; because the modules have no (interesting) top level, but we need
; to clear-state! between each module.
(define-syntax-rule (require-all-tests path ...)
  (run-all-tests path ...))


(require-all-tests
 "base/effects.rkt"
 "base/type.rkt"
 "base/term.rkt"
 "base/bool.rkt"
 "base/merge.rkt"
 "base/list.rkt"
 "base/vector.rkt"
 "base/forall.rkt"
 "base/ord-dict.rkt"
 "base/bitvector.rkt"
 "base/equality.rkt"
 "base/uninterpreted.rkt"
 "base/real.rkt"
 "base/quantified.rkt"
 "base/finitize.rkt"
 "base/distinct.rkt"
 "base/generics.rkt"
 "query/solve.rkt"
 "query/verify.rkt"
 "query/synthesize.rkt"
 "query/solve+.rkt"
 "query/synthax.rkt"
 "query/push-pop.rkt"
 "query/debug.rkt"
 "query/optimize.rkt")


(define (run-tests-with-solver solver%)
  (let ([slvr (solver%)])
    (parameterize ([current-solver slvr][solver slvr])
      (run-solver-specific-tests (solver-features slvr)))))


(printf "===== Running generic tests =====\n")
(run-generic-tests)

(printf "===== Running Z3 tests =====\n")
(run-tests-with-solver z3)

(when (cvc4-available?)
  (printf "===== Running CVC4 tests =====\n")
  (run-tests-with-solver cvc4))

(when (boolector-available?)
  (printf "===== Running Boolector tests =====\n")
  (run-tests-with-solver boolector))
