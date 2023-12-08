#lang racket
 
(require (only-in rosette solver-features current-solver) "base/solver.rkt"
         rosette/lib/roseunit
         rosette/solver/smt/z3 rosette/solver/smt/cvc4 
         rosette/solver/smt/boolector 
         rosette/solver/smt/cvc5
         rosette/solver/smt/bitwuzla
         rosette/solver/smt/stp
         rosette/solver/smt/yices-smt2
         "config.rkt")
(error-print-width default-error-print-width)


; Require all the modules with tests. No tests will actually run,
; because the modules have no (interesting) top level, but we need
; to clear-state! between each module.
(define-syntax-rule (require-all-tests path ...)
  (run-all-tests path ...))


(require-all-tests
;  "base/type.rkt"
;  "base/term.rkt"
;  "base/bool.rkt"
;  "base/merge.rkt"
;  "base/store.rkt"
;  "base/vc.rkt"
;  "base/eval-guarded.rkt"
;  "base/list.rkt"
;  "base/vector.rkt"
;  "base/bvseq.rkt"
;  "base/forall.rkt"
 "base/bitvector.rkt"
;  "base/bvlib.rkt"
;  "base/equality.rkt"
;  "base/uninterpreted.rkt"
;  "base/real.rkt"
;  "base/quantified.rkt"
;  "base/finitize.rkt"
;  "base/distinct.rkt"
;  "base/generics.rkt"
;  "base/push-pop.rkt"
;  "base/optimize-order.rkt"
;  "base/reflect.rkt"
;  "base/decode.rkt"
;  "query/solve.rkt"
;  "query/verify.rkt"
;  "query/synthesize.rkt"
;  "query/solve+.rkt"
;  "query/synthax.rkt"
;  "query/grammar.rkt"
;  "query/optimize.rkt"
;  "lib/destruct.rkt"
;  "profile/test.rkt"
 "trace/test.rkt")


(define (run-tests-with-solver solver%)
  (let ([slvr (solver%)])
    (parameterize ([current-solver slvr][solver slvr])
      (run-solver-specific-tests (solver-features slvr)))))


(define (fast-tests)
  (printf "===== Running generic tests =====\n")
  (run-generic-tests)

  (printf "===== Running Z3 tests =====\n")
  (run-tests-with-solver z3)
  
  (check-all-tests-executed)
  )

(module+ fast
  (fast-tests))


(define (slow-tests)
  ; (when (cvc4-available?)
  ;   (printf "===== Running CVC4 tests =====\n")
  ;   (run-tests-with-solver cvc4))

  ; (when (boolector-available?)
  ;   (printf "===== Running Boolector tests =====\n")
  ;   (run-tests-with-solver boolector))

  ; (when (cvc5-available?)
  ;   (printf "===== Running cvc5 tests =====\n")
  ;   (run-tests-with-solver cvc5))

  ; (when (bitwuzla-available?)
  ;   (printf "===== Running bitwuzla tests =====\n")
  ;   (run-tests-with-solver bitwuzla))
  (when (stp-available?)
    (printf "===== Running stp tests =====\n")
    (run-tests-with-solver stp))
  (when (yices-smt2-available?)
    (printf "===== Running yices-smt2 tests =====\n")
    (run-tests-with-solver yices-smt2))
)

(module+ test
  ; (fast-tests)
  (slow-tests)
  )
