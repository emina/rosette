#lang racket

(require (only-in "smtlib2.rkt" assert minimize maximize
                  check-sat get-model get-unsat-core
                  read-solution true false)
         "env.rkt" "enc.rkt" "dec.rkt"
         (only-in "../../base/core/term.rkt" constant? term-type solvable-default)
         (only-in "../../base/core/function.rkt" fv function? function-domain function-range)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         "../solution.rkt")

(provide encode encode-for-proof decode)

; Given an encoding environment and a list of asserts, minimization objectives,
; and maximization objective, the encode procedure prints an SMT encoding of the given assertions, 
; with respect to the given environment, to current-output-port. 
; In particular, the encoder will not emit any declarations or definitions for Rosette 
; values that appear in the given assertions and that are 
; already bound in the environment.  The environment will 
; be augmented, if needed, with additional declarations and 
; definitions.  This procedure will not emit any other commands.
(define (encode env asserts mins maxs)
  (for ([a asserts])
    (assert (enc a env)))
  (for ([m mins])
    (minimize (enc m env)))
  (for ([m maxs])
    (maximize (enc m env))))

; Given an encoding environment and a list of asserts, 
; the encode-labeled procedure prints an SMT encoding of the given assertions 
; to current-output-port, ensuring that each printed assertion is named. 
; This procedure expects the assertions to be unsatifisable, and the underlying 
; solver's unsat-core-extraction option to be set.  The environment will be augmented, 
; if needed, with additional declarations and definitions.
; This procedure will not emit any other commands.
(define (encode-for-proof env asserts)
  (for ([a asserts])
    (define id (enc a env))
    (assert id (id->name id))))

; Generates an assertion label for a declared or defined SMT id by prefixing that 
; id with 'a'.  This will generate unique ids, since none of the declared or defined 
; constants start with 'a' (see env.rkt).
(define (id->name id)
  (string->symbol (format "a~a" (symbol->string id))))

; Converts an assertion label (produced by id->name) to a declared or defined SMT id 
; by stripping off the first character ('a').
(define (name->id name)
  (string->symbol (substring (symbol->string name) 1)))

; Given an encoding enviornment, the decode procedure reads 
; the solution from current-input-port and converts it into a 
; Rosette solution object.  The port must be connected to a 
; solver working on a problem P such that every identifier 
; declared or defined in P is bound in env.
(define (decode env)
  (match (read-solution)
    [(? hash? sol) 
     (sat (decode-model env sol))]
    [(? list? names)
     (unsat (let ([core (apply set (map name->id names))])
              (for/list ([(bool id) (in-dict env)] #:when (set-member? core id)) 
                 (if (constant? bool) bool (car bool)))))]
    [#f (unsat)]
    ['unknown (unknown)]))

