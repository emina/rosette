#lang racket

; Utilities for testing Rosette programs.
(require rackunit rackunit/text-ui)
(require (only-in rosette 
                  clear-state!
                  current-bitwidth term-cache current-oracle oracle with-asserts-only
                  solution? sat? unsat?))
(require (for-syntax syntax/parse))

(provide run-all-tests test-groups test-suite+ run-generic-tests run-solver-specific-tests
         test-sat test-unsat check-sol check-sat check-unsat)

; Groups tests into N modules with names id ..., each 
; of which requires the specified modules and submodules.
; For example, (test-groups [test fast] "a.rkt" (submod "b.rkt")) 
; creates two module+ forms, test and fast, both of which require 
; "a.rkt" and (submod "b.rkt" id).
(define-syntax (test-groups stx)
  (syntax-case stx ()
    [(_ [id ...] mod ...)
     (quasisyntax/loc stx
       (begin
         #,@(for/list ([i (syntax->list #'(id ...))])
              (quasisyntax/loc i
                (module+ #,i
                  (run-all-tests
                   #,@(for/list ([m (syntax->list #'(mod ...))])
                        (syntax-case m (submod)
                          [(submod name) (quasisyntax/loc m (submod name #,i))]
                          [_ m]))))))))]))


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
             (clear-state!)) ...
           (require 'id) ...))))))
     

; Makes sure that a test suite clears all Rosette state after it terminates.
(define-syntax (test-suite+ stx)
  (syntax-parse stx
    [(_ name:expr
        (~optional (~seq #:features features:expr))
        (~optional (~seq #:before before:expr))
        (~optional (~seq #:after after:expr))
        test:expr ...)
     (with-syntax ([features (or (attribute features) #''())]
                   [before (or (attribute before) #'void)]
                   [after (or (attribute after) #'void)])
       #'(let ([ts (test-suite
                    name
                    #:before (thunk (printf "~a\n" name) (before))
                    #:after after
                    (with-asserts-only
                      (parameterize ([current-bitwidth (current-bitwidth)]
                                     [term-cache (hash-copy (term-cache))]
                                     [current-oracle (oracle (current-oracle))])
                        test ...)))])
           (let ([rts (rosette-test-suite features ts (hash-copy (term-cache)) (current-bitwidth) (oracle (current-oracle)))])
             (set-box! discovered-tests (append (unbox discovered-tests) (list rts)))
             ts)))]))


; Tests discovered by instantiating test-suite+.
; Each element of the list is a rosette-test-suite?.
; A test should only be run if the current-solver satisfies the test's feature list.
(define discovered-tests (box '()))
(struct rosette-test-suite (features ts term-cache bitwidth oracle))


; Run all discovered tests that the given list of features satisfies.
; Only tests that actually require features are run.
(define (run-solver-specific-tests [features '()])
  (for ([rts (in-list (unbox discovered-tests))])
    (match-define (rosette-test-suite feats ts tc bw o) rts)
    (when (and (not (null? feats)) (for/and ([f feats]) (member f features)))
      (parameterize ([term-cache tc]
                     [current-bitwidth bw]
                     [current-oracle o])
        (time (run-tests ts))))))

; The same as run-all-discovered-tests, but only for tests
; that require no features.
(define (run-generic-tests)
  (for ([rts (in-list (unbox discovered-tests))])
    (match-define (rosette-test-suite feats ts tc bw o) rts)
    (when (null? feats)
      (parameterize ([term-cache tc]
                     [current-bitwidth bw]
                     [current-oracle o])
        (time (run-tests ts))))))

    
(define-syntax-rule (check-sol pred test)
  (let ([sol test])
    (check-true (pred sol) (format "not ~a for ~a: ~a" (quote pred) (quote test) sol))
    sol))

(define-syntax-rule (check-sat test) (check-sol sat? test))
(define-syntax-rule (check-unsat test) (check-sol unsat? test))

(define-syntax-rule (test-sat name expr)
  (test-case name (check-sat expr)))

(define-syntax-rule (test-unsat name expr)
  (test-case name (check-unsat expr)))


