#lang racket

(require "core.rkt" 
         (only-in "../base/core/reflect.rkt" symbolics)
         (only-in "../base/core/bool.rkt" ! || vcgen-eval @assume @assert))

(provide solve verify synthesize optimize current-solver (rename-out [∃-solve+ solve+])
         requires ensures)

; Evaluates the given forms and returns the result.
; All assumptions and assertions generated during the
; evaluation of these forms are treated as assumptions.
; In particular, they are all added to the current assumption stack.
(define-syntax-rule (requires form ...)
  (let-values ([(result assumes asserts) (vcgen-eval (begin form ...))])
    (for ([vc (in-sequences (reverse assumes) (reverse asserts))])
      (@assume vc))
    result))

; Evaluates the given forms and returns the result.
; All assumptions and assertions generated during the
; evaluation of these forms are treated as assertions.
; In particular, they are all added to the current assertion stack.
(define-syntax-rule (ensures form ...)
  (let-values ([(result assumes asserts) (vcgen-eval (begin form ...))])
    (for ([vc (in-sequences (reverse assumes) (reverse asserts))])
      (@assert vc))
    result))

; The solve query evaluates the given forms, gathers all 
; assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies those assertions. 
; If current-bitwidth is a positive integer k, and the solve form returns unsat, 
; this means that there is no solution under the k-bit semantics that 
; corresponds to a solution under the infinite precision semantics.  
(define-syntax-rule (solve form forms ...)
  (let-values ([(assumes asserts) (vcs (thunk form forms ...))])
    (∃-solve (append assumes asserts))))

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
    [(_ form)
     (let-values ([(assumes asserts) (vcs (thunk form))])
       (∃-solve `(,@assumes ,(apply || (map ! asserts)))))]))


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
     (let*-values ([(obj) opt] ; evaluate objective first to push its assertions onto the stack
                   [(assumes asserts) (vcs (thunk form))])
       (∃-solve (append assumes asserts) kw obj))]
    [(_ kw1 opt1 kw2 opt2 #:guarantee form)
     (let*-values ([(obj1) opt1]
                   [(obj2) opt2] ; evaluate objectives first to push their assertions onto the stack
                   [(assumes asserts) (vcs (thunk form))])
       (∃-solve (append assumes asserts) kw1 obj1 kw2 obj2))]))