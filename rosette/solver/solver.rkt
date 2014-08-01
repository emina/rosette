#lang racket

(provide solver<%> send/handle-breaks)

; The solver interface specifies the set of functions that 
; should be provided by a Rosette solver. These include 
; assert, clear, solve, solve-all, verify, debug and shutdown. 
; All implementation of the solver interface must provide a 
; no-arguments constructor.  Implementations must also provide a 
; one-argument copy constructor that takes as input a solver and produces 
; a fresh solver instance with no assertions but with the same 
; settings as the provided solver, if applicable.
;
; The assert function takes as input zero or more @boolean? 
; values, each of which encodes a path to an assertion in the program. 
; These assertions are added to the current state of the solver. 
; The clear function clears all assertions from the current state of the solver.
;
; The solve function translates the current list of assertions 
; to a formula that is true iff the program has an execution in which all 
; assertions are true.  The resulting solution, if satisfiable, 
; represents such a successful execution.  The solve-all function
; returns a generator that enumerates all successful executions (if any) 
; for the current set of assertions.  The last solution produced by the 
; generator is unsatisfiable.  If #f is passed to the generator at any point 
; during iteration, the generation of solutions is stopped.
; 
; The debug function translates the current list of assertions to a formula 
; that must be unsatisfiable.  The assertions are expected to  
; encode an execution of the program on a given input and output
; that violates one of the assertions.  The resulting solution
; consists of a minimal unsatisfiable core comprised of program 
; expressions that explain why the execution fails.
; 
; The shutdown function terminates the current solving process (if any), and 
; releases all system resources associated with this solver instance.  The
; solver must be able to reacquire these resources if needed.  That is, the 
; solver should behave as specified above after a shutdown call.
(define solver<%>
  (interface ()
    
    ; (->* () () #:rest (listof @boolean?) void)
    assert
    
    ; (-> void)
    clear
    
    ; (-> solution?)
    solve 
    
    ; (-> generator?)
    solve-all
       
    ; (-> solution?)
    debug
    
    ; (-> void)
    shutdown
    )) 

; Calls the specified method of the given solver.  If a break occurs during 
; execution, it is caught, the cleanup function is called, and the break 
; exception is re-raised. If the cleanup function is #f, the default cleanup 
; procedure shuts down the given solver.
(define-syntax send/handle-breaks
  (syntax-rules ()
    [(_ solver method) (send/handle-breaks solver method #f)]
    [(_ solver method cleanup)
     (with-handlers ([exn:break? (lambda (e) (if cleanup (cleanup) (send solver shutdown)) (raise e))])
       (send solver method))]))

