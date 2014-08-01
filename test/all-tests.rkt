#lang racket

(require "stats/stats.rkt")
(require rosette/config/log)
(current-log-handler (log-handler #:info none/c))

(define (run-all)
  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(require "base/effects.rkt"
                    "base/term.rkt"
                    "base/bool.rkt"
                    "base/num.rkt"
                    "base/list.rkt"
                    "base/equality.rkt"
                    "base/merge.rkt"
                    "solver/kodkod.rkt"))))

(define (run-z3)
  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(require "solver/z3.rkt"))))

(define (run-bv)
  (parameterize ([current-namespace (make-base-namespace)])
    (eval '(require "solver/bvsemantics.rkt"))))

(run/time/log (run-all) "stats/all-tests.txt")
(run/time/log (run-z3)  "stats/all-tests.txt")
(run/time/log (run-bv)  "stats/all-tests.txt")
