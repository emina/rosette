#lang racket

(require rosette/lib/profile/compile
         rosette/lib/profile/record
         racket/runtime-path
         "renderer.rkt")


(filtering-threshold 0)


; Tests must dynamically require their code so that they are seen by the
; profiler compile handler.
(define (run-profile-regression-test path)
  (parameterize ([current-compile symbolic-profile-compile-handler])
    (dynamic-require `(file ,(path->string path)) #f)))


; Test exception handling
(define-runtime-path exn.rkt "benchmarks/exn.rkt")
(define micro-exn
  (regression-test
   "Profiler call graph: benchmarks/exn.rkt"
   "micro-exn"
   (run-profile-regression-test exn.rkt)))

; Test list update-at
(define-runtime-path update-at.rkt "benchmarks/update-at.rkt")
(define micro-update-at
  (regression-test
   "Profiler call graph: benchmarks/update-at.rkt"
   "micro-update-at"
   (run-profile-regression-test update-at.rkt)))


(module+ test
  (require rackunit/text-ui)
  (run-tests micro-exn)
  (run-tests micro-update-at))
