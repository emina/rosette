#lang racket

(require "../base/core/term.rkt" 
         (only-in "../base/core/term.rkt" type-of))

(provide solution? sat? unsat? sat unsat model core)

; Represents the solution to a set of logical constraints.  The solution 
; has single field, result, which stores either a model of the constraints, 
; an unsatisfiable core, or #f. 
; 
; If the constraints are satisfiable, the result is dictionary binding symbolic 
; constants to values. In that case, the solution 
; can be used as a procedure, which takes as input a symbolic constant and 
; returns a concrete value for that constant, if any, or the constant itself, if the 
; model has no binding for that constant.
; 
; If the constraints are unsatisfiable, and no core has been extracted, the result field is #f.
; If a core has been extracted, the result is a list of constraints (that is @boolean? terms or 
; values) that do not have a model.
(struct solution (result)
  #:property prop:procedure 
  (lambda (self arg)
    (match self
      [(solution (? dict? model))
       (if (dict-has-key? model arg)
           (dict-ref model arg)
           arg)]
      [else (error 'solution "cannot query an unsat solution: ~s" self)]))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match (solution-result self)
       [(? dict? model) 
        (let ([bindings (sort (dict->list model) term<? #:key car)])
          (fprintf port "(model")
          (unless (null? bindings)
            (for ([binding bindings])
              (fprintf port "\n [~a ~a]" (car binding) (cdr binding))))
          (fprintf port ")"))]
       [#f (fprintf port "(unsat)")]
       [(list #f) (fprintf port "(core #f)")]
       [core 
        (fprintf port "(core")
        (for ([assertion (sort core term<?)]) 
          (fprintf port "\n ~a" assertion))
        (fprintf port ")")]))])

(define (sat? sol) (and (solution? sol) (dict? (solution-result sol))))

(define (unsat? sol) (not (sat? sol)))

(define (get-model s)
  (match s
    [(solution (? dict? m)) m]
    [_ (error 'model "expected a sat? solution, given ~a" s)]))

(define (get-core s)
  (match s
    [(solution #f) #f]
    [(solution (? list? c)) c]
    [_ (error 'model "expected an unsat? solution, given ~a" s)]))

(define-match-expander model
  (syntax-rules ()
    [(model) (solution (? dict? (app dict-count 0)))]
    [(model pat) (solution (? dict? pat))])
  (syntax-id-rules (set!)
    [(model s) (get-model s)]
    [model get-model]))

(define-match-expander core
  (syntax-rules ()
    [(core) (solution #f)]
    [(core #f) (solution #f)]
    [(core pat) (solution (? list? pat))])
  (syntax-id-rules (set!)
    [(core s) (get-core s)]
    [core get-core]))

(define empty-sat (solution (hash)))
(define empty-unsat (solution #f)) 

; Creates and returns a satisfiable solution consisting of the given model.  The model
; must be an immutable dictionary, with symbolic constants as keys.
(define sat
  (case-lambda 
    [() empty-sat]
    [(model) (unless (and (dict? model) (not (dict-mutable? model)))
               (error 'sat "expected an immutable dictionary, given ~s" model))
             (solution model)]))

; Creates and returns a new unsatisfiable solution that consists of the given core, if any, 
; or no core, if called with no arguments.  The must be a list of @boolean? values.
(define unsat 
  (case-lambda [() empty-unsat]
               [(core) (unless (and (list? core) (not (null? core)))
                         (error 'unsat "expected a non-empty list, given ~s" core))
                       (solution core)]))
  