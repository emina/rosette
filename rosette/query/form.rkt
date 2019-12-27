#lang racket

(require "core.rkt" 
         (only-in "../base/core/reflect.rkt" symbolics)
         (only-in "../base/core/bool.rkt" ! || asserts))

(provide solve verify synthesize optimize
         current-solver (rename-out [∃-solve+ solve+]))

; The solve query evaluates the given expression, gathers all 
; assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies those assertions. 
; If current-bitwidth is a positive integer k, and the solve form returns unsat, 
; this means that there is no solution under the k-bit semantics that 
; corresponds to a solution under the infinite precision semantics.  
(define-syntax-rule (solve expr)
  (∃-solve `(,@(asserts) ,@(eval/asserts (thunk expr)))))

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
     (∃-solve `(,@(asserts)
                ,@(eval/asserts (thunk pre)) 
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
               `(,@(asserts) ,@(eval/asserts (thunk pre))) 
               (eval/asserts (thunk post)))]    
    [(_ #:forall inputs #:guarantee post)
     (synthesize #:forall inputs #:assume #t #:guarantee post)]
    [(_ inputs post)
     (synthesize #:forall inputs #:guarantee post)]))

; The optimize query evaluates the given form, gathers all 
; assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies those assertions and is
; optimal with respect to the given objectives. 
; If current-bitwidth is a positive integer k, and the solve form returns unsat, 
; this means that there is no solution under the k-bit semantics that 
; corresponds to a solution under the infinite precision semantics.  
(define-syntax optimize
  (syntax-rules ()
    [(_ kw opt #:guarantee form)
     (let ([obj opt]) ; evaluate objective first to push its assertions onto the stack
       (∃-solve `(,@(asserts) ,@(eval/asserts (thunk form))) kw obj))]
    [(_ kw1 opt1 kw2 opt2 #:guarantee form)
     (let ([obj1 opt1]
           [obj2 opt2]) ; evaluate objectives first to push their assertions onto the stack
       (∃-solve `(,@(asserts) ,@(eval/asserts (thunk form))) kw1 obj1 kw2 obj2))]))