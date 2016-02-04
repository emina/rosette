#lang racket

(require "util.rkt" "state.rkt" 
         (only-in "../base/core/bool.rkt" with-asserts-only))

(provide solve)

; The solve query evaluates the given formd, gathers all 
; assertions generated during the evalution, 
; and searches for a model (a binding from symbolic 
; constants to values) that satisfies those assertions.
(define-syntax-rule (solve form forms ...)
  (let ([φs (with-handlers ([exn:fail? all-false])
              (with-asserts-only (let () form forms ...)))])
    (unless (all-true? φs (current-solution)) 
      (send (current-solver) clear)
      (current-solution (∃-solve φs #:solver (current-solver)))
      (send (current-solver) clear))
    (current-solution)))

(define all-false (const '(#f)))