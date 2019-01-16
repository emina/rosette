#lang racket

(require racket/generic)

(provide gen:solver solver? 
         solver-assert solver-push solver-pop solver-clear
         solver-minimize solver-maximize
         solver-check solver-debug 
         solver-shutdown solver-features solver-options
         prop:solver-constructor solver-constructor
         )

; The generic solver interface specifies the set of procedures that 
; should be provided by a Rosette solver. These include 
; solver-assert, solver-clear, solver-minimize, solver-maximize,
; solver-check, solver-debug, solver-shutdown, and solver-features. A solver may support
; a subset of this functionality.  This interface loosely follows
; the [SMTLib solver interface](http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf).
;
; A solver uses an assertion stack to keep track of all assertions added to it via
; solver-assert. This stack is partitioned into levels, with each level containing
; a set of assertions. The first (0) assertion level cannot be removed, but more levels
; can be created and removed using the solver-push and solver-pop procedures.  
; 
; The solver-assert procedure takes as input zero or more @boolean? 
; values and adds them to the current level in the assertion stack.
; The solver-push procedure creates a new level in the assertion stack.
; The solver-pop procedure pops the given number of levels off the stack. 
; The solver-clear procedure clears the assertion stack of all levels,
; all assertions, and all objectives.
;
; The solver-minimize and solver-maximize procedures accept 
; numeric terms (@integer?, @real?, or bitvector?) that represent objectives
; to minimize and/or maximize.
;
; The solver-check procedure searches for a solution to the conjunction of 
; the current set of constraints.  If the constraints are satisfiable, 
; the resulting solution is sat?; otherwise it is unsat?.
; 
; The solver-debug procedure searches for an unsatisfiable core for the current 
; set of constraints.  It throws an error if these constraints are 
; satisfiable.  The solver-debug procedure will only core perform extraction on  
; constraints that were added to the solver _after_ the most recent call to 
; solver-check (if any).  All constraints added prior to that call are ignored.
; 
; The solver-shutdown procedure terminates the current solving process (if any), 
; clears all added constraints, and releases all system resources associated 
; with this solver instance.  The solver must be able to reacquire these resources 
; if needed.  That is, the solver should behave as specified above after a shutdown call.
;
; The solver-features procedure returns a list of symbol?s specifying the
; SMT features (logics, optimization, etc) a solver supports.
;
; The solver-options procedure returns a hash table of options the solver
; is configured with.
(define-generics solver
  [solver-assert solver bools]
  [solver-push solver]
  [solver-pop solver [k]]
  [solver-clear solver]
  [solver-minimize solver nums]
  [solver-maximize solver nums]
  [solver-check solver]
  [solver-debug solver]
  [solver-shutdown solver]
  [solver-features solver]
  [solver-options solver])

; Solvers should implement the prop:solver-constructor type property
; to provide the procedure used to construct new solvers of the same type.
; Query forms will use this property to spawn new solvers when necessary
; (e.g., synthesize needs two solvers).
(define-values
  (prop:solver-constructor solver-constructor? get-solver-constructor)
  (make-struct-type-property 'solver-constructor))

(define (solver-constructor solver)
  (make-keyword-procedure
    (lambda (kws kw-args . rest)
      (keyword-apply (get-solver-constructor solver) kws kw-args rest))
    (lambda args
      (apply (get-solver-constructor solver) solver args))))
