#lang racket

(require "util.rkt" "state.rkt" 
         (only-in "../base/core/bool.rkt" ! ||)
         (only-in "../solver/solution.rkt" sat unsat))

(provide solve verify)

(define return-unsat (const (unsat)))

; The solve query evaluates the given forms, gathers all 
; assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies those assertions. 
; If current-bitwidth is a positive integer k, and the solve form returns unsat, 
; this means that there is no solution under the k-bit semantics that 
; corresponds to a solution under the infinite precision semantics.  
(define-syntax-rule (solve form forms ...)
  (∃-solve (eval/asserts (thunk form forms ...))))

; The verify query evaluates the given forms, gathers all 
; assumptions and assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies all the assumptions and 
; violates at least one of the assertions. 
; If current-bitwidth is a positive integer k, and the verify form returns unsat, 
; this means that there is no solution under the k-bit semantics that 
; corresponds to a solution under the infinite precision semantics.  
(define-syntax verify
  (syntax-rules ()
    [(_ #:assume pre #:guarantee post)
     (∃-solve `(,@(eval/asserts (thunk pre)) 
                ,(apply || (map ! (eval/asserts (thunk post))))))]
    [(_ #:guarantee post) (verify #:assume #t #:guarantee post)]
    [(_ post) (verify #:assume #t #:guarantee post)]))



