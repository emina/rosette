#lang racket

; Utilities for testing Rosette programs.
(require rackunit)
(require (only-in rosette clear-state! current-bitwidth solution? sat? unsat?))

(provide test-suite+ test-sat test-unsat check-sat check-unsat)

; Makes sure that a test suite clear all Rosette state after it terminates.
(define-syntax test-suite+
  (syntax-rules ()
    [(_ name #:before before #:after after test ...)
     (test-suite 
      name
      #:before (thunk 
                (printf "~a\n" name)  
                (before)) 
      #:after (thunk 
               (after) 
               (clear-state!)) 
      test ...)]
    [(_ name #:before before test ...)
     (test-suite+ name #:before before #:after void test ...)]
    [(_ name #:after after test ...)
     (test-suite+ name #:before void #:after after test ...)]
    [(_ name test ...)
     (test-suite+ name #:before void #:after void test ...)]))
    
(define satisfiable? (and/c solution? sat?))
(define unsatisfiable? (and/c solution? unsat?))

(define (check-sat v [msg ""]) (check-pred satisfiable? v msg))
(define (check-unsat v [msg ""]) (check-pred unsatisfiable? v msg))

(define-syntax-rule (test-sat name expr)
  (test-case name (check-sat expr "Not a satisfiable solution.")))

(define-syntax-rule (test-unsat name expr)
  (test-case name (check-unsat expr "Not an unsatisfiable solution.")))


