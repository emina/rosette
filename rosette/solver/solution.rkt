#lang racket

(require "../base/core/term.rkt" 
         (only-in "../base/core/term.rkt" type-of)
         (only-in "../base/core/bitvector.rkt" bv? @bvsle))

(provide solution? sat? unsat? sat unsat model core objectives
         interval? interval interval-min interval-max
         interval-includes-min? interval-includes-max?) 

; Represents the solution to a set of logical constraints.  The solution 
; has two fields: result and objectives.  The result field stores either
; a model of the constraints, an unsatisfiable core, or #f. The objectives
; field maps optimization objective terms, if any, to intervals indicating the best
; values those objectives may take in a given solution.  
; 
; If the constraints are satisfiable, the result is dictionary binding symbolic 
; constants to values. In that case, the solution 
; can be used as a procedure, which takes as input a symbolic constant and 
; returns a concrete value for that constant, if any, or the constant itself, if the 
; model has no binding for that constant.
; 
; If the constraints are unsatisfiable, and no core has been extracted, the result
; field is #f.  If a core has been extracted, the result is a list of constraints
; (that is @boolean? terms or values) that do not have a model. The objectives field
; is #f in the case of unsatisfiability.
(struct solution (result objectives)
  #:property prop:procedure 
  (lambda (self arg)
    (match self
      [(solution (? dict? model) _)
       (if (dict-has-key? model arg)
           (dict-ref model arg)
           arg)]
      [else (error 'solution "cannot query an unsat solution: ~s" self)]))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match (solution-objectives self)
       [(? dict? objs)
        (unless (dict-empty? objs)
          (fprintf port "(objectives")
          (for ([(k v) (in-dict objs)])
            (fprintf port "\n [~a ~a]" k v))
          (fprintf port ")\n"))]
       [_ (void)])
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

(define (objectives sol) (solution-objectives sol))

(define (get-model s)
  (match s
    [(solution (? dict? m) _) m]
    [_ (error 'model "expected a sat? solution, given ~a" s)]))

(define (get-core s)
  (match s
    [(solution #f _) #f]
    [(solution (? list? c) _) c]
    [_ (error 'model "expected an unsat? solution, given ~a" s)]))

(define-match-expander model
  (syntax-rules ()
    [(model) (solution (? dict? (app dict-count 0)) _)]
    [(model pat) (solution (? dict? pat) _)])
  (syntax-id-rules (set!)
    [(model s) (get-model s)]
    [model get-model]))

(define-match-expander core
  (syntax-rules ()
    [(core) (solution #f #f)]
    [(core #f) (solution #f #f)]
    [(core pat) (solution (? list? pat) #f)])
  (syntax-id-rules (set!)
    [(core s) (get-core s)]
    [core get-core]))

(define empty-sat (solution (hash) (hash)))
(define empty-unsat (solution #f #f)) 

; Creates and returns a satisfiable solution consisting of the given model.  The model
; must be an immutable dictionary, with symbolic constants as keys.
(define sat
  (case-lambda 
    [() empty-sat]
    [(model)
     (unless (and (dict? model) (not (dict-mutable? model)))
       (raise-arguments-error 'sat "expected an immutable dictionary" "model" model))
     (solution model (hash))]
    [(model objs) 
     (unless (and (dict? model) (not (dict-mutable? model))
                  (dict? objs) (not (dict-mutable? objs)))
       (raise-arguments-error 'sat "expected immutable dictionaries"
                              "model" model "objectives" objs))
     (solution model objs)]))

; Creates and returns a new unsatisfiable solution that consists of the given core, if any, 
; or no core, if called with no arguments.  The must be a list of @boolean? values.
(define unsat 
  (case-lambda [() empty-unsat]
               [(core) (unless (and (list? core) (not (null? core)))
                         (error 'unsat "expected a non-empty list, given ~s" core))
                       (solution core #f)]))

(struct interval (min includes-min? max includes-max?)
  #:transparent
  #:guard (lambda (min includes-min? max includes-max? name)
            (unless (boolean? includes-min?)
              (raise-arguments-error name "expected a boolean?" "includes-min?" includes-min?))
            (unless (boolean? includes-max?)
              (raise-arguments-error name "expected a boolean?" "includes-max?" includes-max?))
            (unless (or (and (real? min) (real? max) (<= min max))
                        (and (bv? min) (bv? max) (@bvsle min max)))
              (raise-arguments-error name "expected two real? or bv? values with min <= max"
                                     "min" min "max" max))
            (values min includes-min?  max includes-max?))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-define (interval min includes-min? max includes-max?) self)
     (fprintf port (if includes-min? "[" "("))
     (fprintf port "~a " min)
     (fprintf port "~a" max)
     (fprintf port (if includes-max? "]" ")")))])
  