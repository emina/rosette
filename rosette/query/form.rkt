#lang racket

(require syntax/parse/define
         "core.rkt"
         "../base/tracer/tracer.rkt"
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
(define-simple-macro (solve expr)
  #:with the-syntax this-syntax
  (with-trace* the-syntax (∃-solve `(,@(asserts) ,@(eval/asserts (thunk expr))))))

; The verify query evaluates the given forms, gathers all 
; assumptions and assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies all the assumptions and 
; violates at least one of the assertions. 
; If current-bitwidth is a positive integer k, and the verify form returns unsat, 
; this means that there is no solution under the k-bit semantics that 
; corresponds to a solution under the infinite precision semantics.  
(define-syntax-parser verify
  [(_ #:assume pre #:guarantee post) #`(verify/derived #,this-syntax pre post)]
  [(_ #:guarantee post) #`(verify/derived #,this-syntax #t post)]
  [(_ post) #`(verify/derived #,this-syntax #t post)])

(define-simple-macro (verify/derived orig pre post)
  (with-trace orig
    (∃-solve `(,@(asserts)
               ,@(with-subtrace 'assume
                   (eval/asserts (thunk pre)))
               ,(apply || (map ! (with-subtrace 'guarantee
                                   (eval/asserts (thunk post)))))))))

; The synthesize query evaluates the given forms, gathers all 
; assumptions and assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) of the formula ∃H.∀I. pre => post, 
; where I are the given input constants and H are all other symoblic constants.
(define-syntax-parser synthesize
  [(_ #:forall inputs #:assume pre #:guarantee post)
   #`(synthesize/derived #,this-syntax inputs pre post)]
  [(_ #:forall inputs #:guarantee post)
   #`(synthesize/derived #,this-syntax inputs #t post)]
  [(_ inputs post)
   #`(synthesize/derived #,this-syntax inputs #t post)])

(define-simple-macro (synthesize/derived orig inputs pre post)
  (with-trace orig
    (∃∀-solve (symbolics inputs)
              `(,@(asserts) ,@(with-subtrace 'assume (eval/asserts (thunk pre))))
              (with-subtrace 'guarantee (eval/asserts (thunk post))))))

; The optimize query evaluates the given form, gathers all 
; assertions generated during the evaluation, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies those assertions and is
; optimal with respect to the given objectives. 
; If current-bitwidth is a positive integer k, and the solve form returns unsat, 
; this means that there is no solution under the k-bit semantics that 
; corresponds to a solution under the infinite precision semantics.  
(define-syntax-parser optimize
  [(_ kw opt #:guarantee form)
   #`(let ([obj opt]) ; evaluate objective first to push its assertions onto the stack
       (∃-solve `(,@(asserts) ,@(with-trace* #,this-syntax (eval/asserts (thunk form)))) kw obj))]
  [(_ kw1 opt1 kw2 opt2 #:guarantee form)
   #`(let ([obj1 opt1]
           [obj2 opt2]) ; evaluate objectives first to push their assertions onto the stack
       (∃-solve `(,@(asserts) ,@(with-trace* #,this-syntax (eval/asserts (thunk form)))) kw1 obj1 kw2 obj2))])
