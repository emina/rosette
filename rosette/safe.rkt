#lang racket

(require "solver/solver.rkt" "solver/smt/z3.rkt"
         "solver/solution.rkt" 
         (only-in "solver/smt/server.rkt" output-smt)
         "base/base.rkt"
         "query/query.rkt"
         (for-syntax racket)
         (prefix-in racket/ (only-in racket append append-map map car)))


(define (exported mod)
  (let*-values ([(vars stx) (module->exports  mod)]
                [(all) (racket/append (racket/append-map cdr vars) (racket/append-map cdr stx))])
    (racket/map racket/car all)))

(define (rosette)
  (racket/append (exported 'rosette/solver/solver)
                 (exported 'rosette/solver/solution)
                 (exported 'rosette/base/base)
                 (exported 'rosette/query/query)))

(define (clear-state!)
  (current-bitwidth 5)
  (current-oracle (oracle))
  (clear-asserts!)
  (clear-terms!)
  (current-solver (z3)))

(provide 
 (all-from-out  
  "solver/solver.rkt"
  "solver/solution.rkt" 
  "base/base.rkt"
  "query/query.rkt")
 (for-syntax (all-from-out racket))
 rosette clear-state! output-smt)