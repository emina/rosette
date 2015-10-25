#lang racket

; Utilities for testing Rosette programs.
(require rackunit)
(require (only-in rosette 
                  clear-state!
                  current-bitwidth term-cache current-oracle oracle with-asserts-only
                  current-solution empty-solution solution? sat? unsat?))

(provide run-all-tests test-suite+ test-sat test-unsat check-sat check-unsat)

; Given a set of relative paths containing modules with tests, 
; requires them all into the present environment, one by one, 
; clearing the Rosette state between each import.
(define-syntax (run-all-tests stx)
  (syntax-case stx ()
    ([_ path ...]
     (with-syntax ([(id ...) (generate-temporaries #'(path ...))])
       (syntax/loc stx
         (begin
           (module id racket 
             (require path)
             (require (only-in rosette/safe clear-state!))
             (current-bitwidth 5)
             (clear-state!)) ...
            (require 'id) ...))))))

; Makes sure that a test suite clears all Rosette state after it terminates.
(define-syntax test-suite+
  (syntax-rules ()
    [(_ name #:before before #:after after test ...)
     (test-suite 
      name
      #:before (thunk (printf "~a\n" name) (before)) 
      #:after after
      (with-asserts-only
       (parameterize ([current-bitwidth (current-bitwidth)]
                      [term-cache (hash-copy (term-cache))]
                      [current-oracle (oracle (current-oracle))]
                      [current-solution (empty-solution)])
         test ...)))]
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


