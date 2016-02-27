#lang racket

(require (only-in "smtlib2.rkt" assert minimize maximize
                  check-sat get-model get-unsat-core
                  read-solution true false)
         "env.rkt" "enc.rkt"
         (only-in "../../base/core/term.rkt" type-of)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         (only-in "../../base/struct/enum.rkt" enum? enum-members)
         "../solution.rkt")

(provide encode encode-for-proof decode)

; Given an encoding environment and a list of asserts, minimization objectives,
; and maximization objective, the encode procedure prints an SMT encoding of the given assertions, 
; with respect to the given environment, to current-output-port. 
; In particular, the encoder will not emit any declarations or definitions for Rosette 
; values that appear in the given assertions and that are 
; already bound in the environment.  The environment will 
; be augmented, if needed, with additional declarations and 
; definitions.
(define (encode env asserts mins maxs)
  (for ([a asserts])
    (assert (enc a env)))
  (for ([m mins])
    (minimize (enc m env)))
  (for ([m maxs])
    (maximize (enc m env)))
  (check-sat)
  (get-model))

; Given an encoding environment and a list of asserts, 
; the encode-labeled procedure prints an SMT encoding of the given assertions 
; to current-output-port, ensuring that each printed assertion is named. 
; This procedure expects the assertions to be unsatifisable, and the underlying 
; solver's unsat-core-extraction option to be set.  The environment will be augmented, 
; if needed, with additional declarations and definitions.
(define (encode-for-proof env asserts)
  (for ([a asserts])
    (define id (enc a env))
    (assert id (id->name id)))
  (check-sat)
  (get-unsat-core))

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
; declared or defined in P is bound in (decls env) or (defs env), respectively.
(define (decode env)
  (match (read-solution)
    [(? hash? sol) 
     (sat (for/hash ([(const id) (in-dict (decls env))])
            (values const 
                    (if (hash-has-key? sol id)
                        (decode-binding const (hash-ref sol id))
                        (default-binding const)))))]
    [(? list? names)
     (unsat (let ([core (apply set (map name->id names))])
              (for/list ([(bool id) (in-sequences (in-dict (decls env)) (in-dict (defs env)))]
                          #:when (set-member? core id)) 
                 bool)))]
    [#f (unsat)]))

(define (to-exact-int a) (if (integer? a) (inexact->exact a) a))

(define (decode-binding const val)
  (match (type-of const)
    [(== @boolean?)
     (match val
       [(== true) #t]
       [(== false) #f]
       [_ (error 'decode-binding "expected 'true or 'false binding for ~a, given ~a" const val)])]
    [(== @integer?) 
     (match val
       [(? integer?) val]
       [(list '- v) (- v)])]
    [(== @real?) 
     (match val 
       [(? real?) val]
       [(list '- (list '/ a b)) (- (/ (to-exact-int a) (to-exact-int b)))]
       [(list '- v) (- v)]
       [(list '/ a b) (/ (to-exact-int a) (to-exact-int b))]
       [(list '/ (list '- a) b) (/ (- (to-exact-int b)) (to-exact-int b))])]
    [(? bitvector? t)
     (match val
       [(? number?) (bv val t)]
       [(list _ (app symbol->string (regexp #px"bv(\\d+)" (list _ (app string->number n)))) _)
        (bv n t)])]
    [(? enum? t) (vector-ref (enum-members t) val)]
    [other other]))
