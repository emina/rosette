#lang racket

(require (for-syntax racket/syntax) 
         racket/stxparam 
         (only-in "core.rkt" current-solver ∃-debug eval/asserts)
         "../lib/util/syntax-properties.rkt"  
         "../base/form/define.rkt" "../base/core/bool.rkt"  "../base/form/state.rkt" 
         "../base/core/equality.rkt" "../base/core/term.rkt")

(provide relax? relate debug-origin debug define/debug protect true false)

(define-syntax debug
  (syntax-rules ()
    [(debug form) 
     (parameterize ([current-oracle (oracle)])
       (∃-debug (eval/asserts (thunk form))))]
    [(debug [pred other ...] form)
     (parameterize ([relax? (list pred other ...)])
       (debug form))]))

(define-syntax-rule (define/debug head body ...) 
  (define head (syntax-parameterize ([relax relax/assert]) body ...)))

(define-syntax true
  (syntax-id-rules (set!)
    [(set! true e) (error 'set! "cannot reassign the constant true")]
    [true (relax #t true)]))

(define-syntax false
  (syntax-id-rules (set!)
    [(set! false e) (error 'set! "cannot reassign the constant false")]
    [false (relax #f false)]))

(define-syntax-rule (protect expr)
  (syntax-parameterize ([relax (syntax-rules () [(_ form loc) form])]) expr))

(define relax?
  (make-parameter none/c
                  (lambda (pred)
                    (unless (or (type? pred) (and (list? pred) (andmap type? pred)))
                      (error 'relax? "expected a type or list of types, given ~s" pred))
                    (match pred
                      [(or (? type? p) (list p)) (lambda (v) (eq? (type-of v) p))]
                      [_ (lambda (v) (not (false? (memq (type-of v) pred))))]))))

(define relate
  (make-parameter @equal?
                  (lambda (rel)
                    (unless (and (procedure? rel) (procedure-arity-includes? rel 2))
                      (error 'relate "expected a 2 argument procedure, given ~s" rel))
                    rel)))

(define-for-syntax (relax/assert stx)
  (syntax-case stx ()
    [(_ val origin) 
     #`(call-with-values 
        (thunk val) 
        (relax-values (syntax/source origin)))]))

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



