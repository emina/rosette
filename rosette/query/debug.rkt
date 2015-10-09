#lang racket

(require (for-syntax racket/syntax) 
         racket/stxparam 
         "../lib/util/syntax-properties.rkt"  
         "../base/form/define.rkt" "../base/core/assert.rkt" "state.rkt" "../base/form/state.rkt" 
         "../solver/solver.rkt" "../solver/solution.rkt" 
         "../base/core/equality.rkt" "../base/core/term.rkt")

(provide relax? relate relaxer? relaxed-by debug define/debug protect true false)

(define-syntax debug
  (syntax-rules ()
    [(debug form) 
     (parameterize ([current-oracle (oracle)])
       (let ([asserts (with-asserts-only form)])
         (send/apply (current-solver) assert asserts)
         (let ([sol (send/handle-breaks (current-solver) debug)])
           (send (current-solver) clear)
           (unless (unsat? sol)
             (error 'debug "found a solution instead of a minimal core: ~a" sol))
           sol)))]
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
;     #`(call-with-values 
;        (lambda () val) 
;        (relax-values #'origin))]))
     #`(call-with-values 
        (lambda () val) 
        (relax-values (syntax/source origin)))]))

(define (relax-values origin)
  (lambda rs 
    (apply values 
           (map (lambda (r) 
                  (if (and ((relax?) r) (not (relaxer? r)))
                      (local [(define-symbolic* relaxer (type-of r))
                              (define tracked 
                                (term-property 
                                 (term-track-origin relaxer origin)
                                 'relaxer? #t))]
                        (@assert (term-property ((relate) tracked r) 'relaxed-by relaxer) 
                                    "" origin)
                        tracked)
                      r))
                rs))))

(define (relaxer? val)   (term-property val 'relaxer?))
(define (relaxed-by val) (term-property val 'relaxed-by))


