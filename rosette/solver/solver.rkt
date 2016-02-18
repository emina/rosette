#lang racket

(require racket/generic)

(provide gen:solver solver? 
         solver-add solver-clear 
         solver-check solver-localize 
         solver-shutdown)

; The generic solver interface specifies the set of procedures that 
; should be provided by a Rosette solver. These include 
; solver-add, solver-clear, solver-check, solver-localize, and solver-shutdown. 
;
; The solver-add procedure takes as input zero or more @boolean? 
; values and adds them to the current state of the solver. 
; The solver-clear procedure clears all constraints from the current 
; state of the solver.
;
; The solver-check procedure searches for a solution to the conjunction of 
; the current set of constraints.  If the constraints are satisfiable, 
; the resulting solution is sat?; otherwise it is unsat?.
; 
; The solver-localize procedure searches for an unsatisfiable core for the current 
; set of constraints.  It throws an error if these constraints are 
; satisfiable.  The server-localize procedure will only perform localization on  
; constraints that were added to the solver _after_ the most recent call to 
; solver-check (if any).  All constraints added prior to that call are ignored.
; 
; The solver-shutdown procedure terminates the current solving process (if any), 
; clears all added constraints, and releases all system resources associated 
; with this solver instance.  The solver must be able to reacquire these resources 
; if needed.  That is, the solver should behave as specified above after a shutdown call.
(define-generics solver
  [solver-add solver bools]
  [solver-clear solver]
  [solver-check solver]
  [solver-localize solver]
  [solver-shutdown solver])
