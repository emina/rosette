#lang racket

(require racket/generic)

(provide gen:solver solver? assert clear shutdown solve debug)

; The generic solver interface specifies the set of procedures that 
; should be provided by a Rosette solver. These include 
; assert, clear, solve, debug, and shutdown. 
;
; The assert procedure takes as input zero or more @boolean? 
; values and adds them to the current state of the solver. 
; The clear procedure clears all assertions from the current 
; state of the solver.
;
; The solve procedure searches for a solution to the conjunction of 
; the current set of assertions.  If the assertions are satisfiable, 
; the resulting solution is sat?; otherwise it is unsat?.
; 
; The debug procedure searches for an unsatisfiable core for the current 
; set of assertions.  It throws an error if the current assertions are 
; satisfiable.
; 
; The shutdown procedure terminates the current solving process (if any), 
; clears all added assertions, and releases all system resources associated 
; with this solver instance.  The solver must be able to reacquire these resources 
; if needed.  That is, the solver should behave as specified above after a shutdown call.
(define-generics solver
  [assert solver bools]
  [clear solver]
  [shutdown solver]
  [solve solver]
  [debug solver])
