#lang racket

(require "../base/core/term.rkt")

(provide solution? sat? unsat?
         sat unsat model core
         unbind solution->list)

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
; If the constraints are unsatisfiable, and no core has been extract, the result field is #f.
; If a core has been extract, the result is a list of constraints (that is @boolean? terms or 
; values) that do not have a model.
(struct solution (result)
  #:property prop:procedure 
  (lambda (self arg)
    (match self
      [(solution (? dict? model)) (dict-ref model arg arg)]
      [else (error 'solution "cannot query an unsat solution: ~s" self)]))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (if mode 
         (display-solution self port)
         (write-solution self port)))])

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

(define (solution->list sol)
  (match (solution-result sol)
    [(? dict? model)  (sort (dict->list model) var<? #:key car)]
    [(? list? core)   (let-values ([(origin no-origin) (partition term-origin core)])
                        (append (sort origin stx<?) no-origin))]
    [#f (list)]))
     

; Given a satisfiable solution, returns a copy of the given solution but without 
; any bindings for variables that satisfy the given predicate.
(define (unbind sol unbind?)
  (match (solution-result sol)
    [(? dict? model) (sat (for/hash ([(var val) model] #:unless (unbind? var))
                            (values var val)))]
    [_ (error 'unbind "cannot remove variable bindings from an unsat solution: ~s" sol)]))
   
(define (write-solution sol port)
  (match (solution-result sol)
    [(? dict? model)
     (let ([trace (dict->list model)])
       (fprintf port "model(")
       (unless (null? trace)
         (fprintf port "[~s ~s]" (term-name (caar trace)) (cdar trace))
         (for ([binding (cdr trace)])
           (fprintf port "\n      [~s ~s]" (term-name (car binding)) (cdr binding))))
       (fprintf port ")"))]
    [core
     (fprintf port "core(")
     (when core
       (fprintf port "[~a ~a]"  (term-origin (car core)) (car core))
       (for ([assertion (cdr core)])
         (fprintf port "\n     [~a ~a]"  (term-origin assertion) assertion)))
     (fprintf port ")")]))

(define (display-solution sol port)
  (match (solution-result sol)
    [(? dict? model) 
     (let ([trace (sort (dict->list model) var<? #:key car)])
       (fprintf port "(model")
       (unless (null? trace)
         (for ([binding trace])
           (fprintf port "\n [~a ~a]" (car binding) (cdr binding))))
       (fprintf port ")"))]
    [core 
     (fprintf port "(core")
     (when core
       (for ([assertion (sort (remove-duplicates (filter-map term-origin core)) stx<?)])
         (fprintf port "\n ~a" assertion)))
     (fprintf port ")")]))

(define (identifier-and-index v)
  (match (term-name v)
    [(cons id idx) (values id idx)]
    [n (values n 0)]))

(define (var<? v0 v1)
  (let*-values ([(v0-stx v0-idx) (identifier-and-index v0)]
                [(v1-stx v1-idx) (identifier-and-index v1)])
    (if (equal? v0-stx v1-stx)
        (< v0-idx v1-idx)
        (stx<? v0-stx v1-stx))))
                     
(define (source->string stx)
  (let ([src (syntax-source stx)])
    (cond [(false? src) ""]
          [(string? src) src]
          [(path? src) (path->string src)]
          [else (~.a src)])))
    
(define (stx<? s0 s1)
  (let* ([src0 (source->string s0)]
         [src1 (source->string s1)])
    (or (string<? src0 src1)
        (and (equal? src0 src1)
             (let ([ln0 (or (syntax-line s0) -1)]
                   [ln1 (or (syntax-line s1) -1)])
               (or (< ln0 ln1)
                   (and (= ln0 ln1)
                        (let ([cl0 (or (syntax-column s0) -1)]
                              [cl1 (or (syntax-column s1) -1)])
                          (< cl0 cl1)))))))))
 
  
                                   
                               
