#lang racket

(require "core.rkt" 
         (only-in "../base/core/reflect.rkt" symbolics)
         (only-in "../base/core/bool.rkt" ! ||))

(provide solve verify synthesize current-solver)

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

; The synthesize query evaluates the given forms, gathers all 
; assumptions and assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) of the formula ∃H.∀I. pre => post, 
; where I are the given input constants and H are all other symoblic constants. 
(define-syntax synthesize 
  (syntax-rules (synthesize)
    [(_ #:forall inputs #:assume pre #:guarantee post)
     (∃∀-solve (symbolics inputs) 
               (eval/asserts (thunk pre)) 
               (eval/asserts (thunk post)))]    
    [(_ #:forall inputs #:guarantee post)
     (synthesize #:forall inputs #:assume #t #:guarantee post)]))