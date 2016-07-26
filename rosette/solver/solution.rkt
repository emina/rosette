#lang racket

(require "../base/core/term.rkt")

(provide solution? sat? unsat? unknown?
         (rename-out [make-sat sat] [make-unsat unsat] [make-unknown unknown])
         model core)

; Represents the solution to a set of logical constraints.
; The contents of a solution depend on whether it is satisfiable,
; unsatisfiable, or unknown.
(struct solution ())

; A satisfiable solution consists of a dictionary that binds symbolic 
; constants to values. Such a solution can be used as a procedure, which takes as input
; a symbolic constant and returns a concrete value for that constant, if any, or the constant
; itself, if the model has no binding for that constant.
(struct sat solution (model)
  #:property prop:procedure 
  (lambda (self arg)
    (let ([model (sat-model self)])
      (if (dict-has-key? model arg)
          (dict-ref model arg)
          arg)))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (let ([bindings (sort (dict->list (sat-model self)) term<? #:key car)])
       (fprintf port "(model")
       (unless (null? bindings)
         (for ([binding bindings])
           (fprintf port "\n [~a ~a]" (car binding) (cdr binding))))
       (fprintf port ")")))])

; If a solution is unsatisfiable, and no core has been extracted, the core field is #f.
; If a core has been extracted, the core is a list of constraints (that is @boolean? terms or 
; values) that do not have a model.
(struct unsat solution (core)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match (unsat-core self)
       [#f (fprintf port "(unsat)")]
       [(list #f) (fprintf port "(core #f)")]
       [core 
        (fprintf port "(core")
        (for ([assertion (sort core term<?)]) 
          (fprintf port "\n ~a" assertion))
        (fprintf port ")")]))])

; An unknown solution is returned when the solver doesn't have a complete
; procedure for deciding a given set of constraints.
(struct unknown solution ()
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(unknown)"))])

(define-match-expander model
  (syntax-rules ()
    [(model) (sat (app dict-count 0))]
    [(model pat) (sat pat)])
  (syntax-id-rules (set!)
    [(model s) (sat-model s)]
    [model sat-model]))

(define-match-expander core
  (syntax-rules ()
    [(core) (unsat #f)]
    [(core pat) (unsat pat)])
  (syntax-id-rules (set!)
    [(core s) (unsat-core s)]
    [core unsat-core]))

(define sat0 (sat (hash)))
(define unsat0 (unsat #f))
(define unknown0 (unknown))

; Returns a satisfiable solution consisting of the given model.  The model
; must be an immutable dictionary, with symbolic constants as keys.
(define make-sat
  (case-lambda 
    [() sat0]
    [(model) (unless (and (dict? model) (not (dict-mutable? model)))
               (error 'sat "expected an immutable dictionary, given ~s" model))
             (sat model)]))

; Returns an unsatisfiable solution that consists of the given core, if any, 
; or no core, if called with no arguments.  The must be a list of @boolean? values.
(define make-unsat 
  (case-lambda [() unsat0]
               [(core) (unless (and (list? core) (not (null? core)))
                         (error 'unsat "expected a non-empty list, given ~s" core))
                       (unsat core)]))

; Returns an unknown solution.
(define (make-unknown) unknown0)
  