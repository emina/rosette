#lang racket

(require (for-syntax racket/syntax) racket/stxparam 
         (only-in "core.rkt" current-solver ∃-debug eval/asserts)
         "../lib/util/syntax-properties.rkt"
         (only-in "../base/form/app.rkt" app)
         "../base/core/bool.rkt"  "../base/form/state.rkt" 
         "../base/core/equality.rkt" "../base/core/term.rkt")

(provide relax? relate debug-origin debug define/debug protect assert)

(define-syntax debug
  (syntax-rules ()
    [(debug form) 
     (parameterize ([current-oracle (oracle)])
       (∃-debug (eval/asserts (thunk form))))]
    [(debug [pred other ...] form)
     (parameterize ([relax? (list pred other ...)])
       (debug form))]))

(define-syntax-rule (define/debug head body ...) 
  (define head
    (syntax-parameterize
     ([app app-track])
     body ...)))

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ expr) (syntax/loc stx (@assert (protect expr) #f))]
    [(_ expr msg) (syntax/loc stx (@assert (protect expr) (protect msg)))]))


(define-for-syntax (app-track stx)
  (syntax-case stx ()
    [(_ proc arg ...) 
     (quasisyntax/loc stx
       (call-with-values (thunk (#%app proc arg ...))
                         (relax-values (syntax/source proc))))]))

(define-syntax-rule (protect expr)
  (syntax-parameterize ([app (syntax-rules () [(_ proc arg (... ...)) (#%app proc arg (... ...))])])
                       expr))

(define relax?
  (make-parameter none/c
                  (lambda (pred)
                    (unless (or (solvable? pred) (and (list? pred) (andmap solvable? pred)))
                      (error 'debug "expected a solvable type or list of solvable types, given ~s" pred))
                    (match pred
                      [(or (? type? p) (list p)) (lambda (v) (eq? (type-of v) p))]
                      [_ (lambda (v) (not (false? (memq (type-of v) pred))))]))))

(define relate
  (make-parameter @equal?
                  (lambda (rel)
                    (unless (and (procedure? rel) (procedure-arity-includes? rel 2))
                      (error 'relate "expected a 2 argument procedure, given ~s" rel))
                    rel)))

(define (relax-values origin)
  (lambda rs 
    (apply values 
           (map (lambda (r) 
                  (if (and ((relax?) r) (not (relaxer? r)))
                      (let ([tracked (constant (list relaxer origin) (type-of r))])
                        (@assert ((relate) tracked r))
                        tracked)
                      r))
                rs))))

(define relaxer #'relaxer)

(define (relaxer? val)
  (match val
    [(constant (list (== relaxer) _) _) #t]
    [_ #f]))

(define (debug-origin val)
  (match val
    [(constant (list (== relaxer) origin) _) origin]
    [_ #f]))



