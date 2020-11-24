#lang racket

(require
  (only-in "term.rkt" term? term-type)
  (only-in "bool.rkt" @boolean? @false? ! || && => pc)
  "vc.rkt" "result.rkt" "store.rkt" "merge.rkt")

(provide speculate)

(define (return-false e) #f)

; Temporary code to integrate and test new store tracking
; module, before implementing VC collection and handling.
; 
; This form extends the PC with the given guard, calls
; (with-store body) under that guard and returns the result,
; if with-store executes normally under the guard. Otherwise,
; this form catches any exn:fail? exception thrown by with-store
; and returns #f. This mimics the behavior old speculate* form.
(define-syntax-rule (speculate guard body)
  (with-handlers ([exn:fail? return-false])
    (with-store (parameterize ([pc guard]) body))))
    