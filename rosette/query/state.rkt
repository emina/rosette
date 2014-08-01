#lang racket


(require (for-syntax racket/syntax)
         racket/stxparam racket/stxparam-exptime
         "../solver/solver.rkt"  "../solver/solution.rkt"  
          "../solver/kodkod/kodkod.rkt")

(provide current-solution
         current-solver)

#|--------------current state parameters--------------|#

(define current-solution 
  (make-parameter (empty-solution)
                  (lambda (sol) 
                    (unless (solution? sol)
                      (error 'current-solution "expected a solution, given ~s" sol))
                    sol)))

(define current-solver
  (make-parameter (new kodkod%)
                  (lambda (solver)
                    (unless (is-a? solver solver<%>)
                      (error 'current-solver "expected a solver<%>, given ~s" solver))
                    (send (current-solver) shutdown)
                    solver)))


