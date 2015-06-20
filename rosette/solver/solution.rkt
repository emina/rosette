#lang racket

(require "../base/term.rkt")

(provide solution? 
         sat unsat 
         empty-solution
         unbind
         unsat?
         solution->list
         [rename-out (solution-sat? sat?)
                     (solution-model model)
                     (solution-core core)] )

; Stores the solution to a given Kodkodi problem.  The sat? field is either #t or #f. 
; The model field is an ordered dictionary; the core field is a list.  The model field is non-false 
; iff the sat? field is #t; otherwise, the core field may be non-false.  The model field, if non-false, 
; binds symbolic variables to values.
; The core field, if non-false, contains a minimal unsatisifable core of the original problem, given as 
; a list of program expressions.  A core is option for unsatisfiable solutions. 
; If the solution is satisfiable, it can be used as a procedure to retrieve the value of a variable in 
; the model.  If the model does not have a binding for that variable, it returns the variable itself.
(struct solution (sat? model core)
  #:property prop:procedure 
  (lambda (self arg) 
    (or (solution-sat? self) (error 'solution "cannot query an unsat solution: ~s" self))
    (dict-ref (solution-model self) arg arg))
    #:methods gen:custom-write
  [(define (write-proc self port mode)
     (if mode 
         (display-solution self port)
         (write-solution self port)))])

(define (unsat? sol) (not (solution-sat? sol)))

; Creates and returns a new satisfiable solution consisting of the given model.  The model
; must be an immutable dictionary, with symbolic variables as keys.
(define (sat model)
  (unless (and (dict? model) (not (dict-mutable? model)))
      (error 'sat "expected an immutable dictionary, given ~s" model))
  ;(log-info (format "SAT: ~s" model))
  (solution #t model #f))

; Creates and returns a new unsatisfiable solution that consists of the given core, if any, 
; or no core, if called with no arguments.  The core must be minimal, and it must be given 
; as a list of program expressions.
(define unsat 
  (case-lambda [() (solution #f #f #f)]
               [(core) (unless (and (list? core) (not (null? core)))
                           (error 'unsat "expected a non-empty list, given ~s" core))
                       (solution #f #f core)]))

; Returns a satisfiable solution with an empty model.
(define empty-solution
  (let ([empty (sat (hash))])
    (lambda () empty)))

(define (solution->list sol)
  (if (unsat? sol)
      (let-values ([(origin no-origin) (partition term-origin (solution-core sol))])
        (append (sort origin stx<?) (no-origin)))
      (sort (dict->list (solution-model sol)) var<? #:key car)))

; Given a satisfiable solution, returns a copy of the given solution but without 
; any bindings for variables that satisfy the given predicate.
(define (unbind sol unbind?)
  (unless (solution-sat? sol)
    (error 'solution-unbind "cannot remove variable bindings from an unsat solution: ~s" sol))
  ;(printf "UNBINDING ~a ~a\n" sol unbind?)
  ;(for ([(var val) (solution-model sol)] #:when (unbind? var))
  ;  (printf "UNBIND ~s FROM ~s\n" var val)) 
  (sat (for/hash ([(var val) (solution-model sol)] #:unless (unbind? var))
         (values var val)))) 

(define (write-solution sol port)   
  (if (solution-sat? sol)
      (let ([trace (dict->list (solution-model sol))])
        (fprintf port "model(")
        (unless (null? trace)
          (fprintf port "[~s ~s]" (term-name (caar trace)) (cdar trace))
          (for ([binding (cdr trace)])
            (fprintf port "\n      [~s ~s]" (term-name (car binding)) (cdr binding))))
        (fprintf port ")"))
      (let ([core (solution-core sol)])
        (fprintf port "core(")
        (when core
          (fprintf port "[~a ~a]"  (term-origin (car core)) (car core))
          (for ([assertion (cdr core)])
            (fprintf port "\n     [~a ~a]"  (term-origin assertion) assertion)))
        (fprintf port ")"))))

(define (display-solution sol port)
  ;(for ([c (solution-core sol)]) (printf "CLAUSE: ~a, ORIGIN: ~a\n" c (term-origin c)))    
  (if (solution-sat? sol)
      (let ([trace (sort (dict->list (solution-model sol)) var<? #:key car)])
        (fprintf port "(model")
        (unless (null? trace)
          (for ([binding trace])
            (fprintf port "\n [~a ~a]" (car binding) (cdr binding))))
        (fprintf port ")"))
      (let* ([core (solution-core sol)]
             [core (and core (sort (remove-duplicates (filter-map term-origin core)) stx<?))])        
        (fprintf port "(core")
        (when core
          (for ([assertion core])
            (fprintf port "\n ~a" assertion)))
        (fprintf port ")"))))

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
 
  
                                   
                               
