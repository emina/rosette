#lang rosette

; A simple test to check exception handling. When running the symbolic profiler
; in trace mode, the `add1` invocation in `foo` should not be a child of the
; `raise-argument-error` invocation. If it is, then the profiler stack has
; fallen out of sync with the actual stack due to the exception being thrown.

(define (foo n)
  (if (= n 2)
      (raise-argument-error 'foo "not 2" n)
      (add1 n)))

(define (bar)
  (define-symbolic i integer?)
  (foo i))

(bar)
