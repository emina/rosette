#lang racket

(require racket/syntax 
         (only-in "smtlib2.rkt" cmd assert check-sat get-model reset read-solution true false)
         "env.rkt" "enc.rkt"
         "../../base/core/term.rkt" 
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/num.rkt" @number?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         (only-in "../../base/struct/enum.rkt" enum? enum-members)
         "../solution.rkt")

(provide encode decode clear-solver)

; Given an encoding environment, a list of asserts, and
; a solver output port, the encode procedure prints an SMT 
; encoding of the given assertions to the given port, with 
; respect to the given environment. In particular, the encoder 
; will not emit any declarations or definitions for Rosette 
; values that appear in the given assertions and that are 
; already bound in the environment.  The environment will 
; be augmented, if needed, with additional declarations and 
; definitions.
(define (encode env asserts port)
  (cmd [port]
    (for ([a asserts])
      (assert (enc a env)))
    (check-sat)
    (get-model)))

; Given an encoding enviornment and a solver input port, 
; the decode procedure reads the solution from the port 
; and converts it into a Rosette solution object.  The 
; port must be connected to a solver working on a problem 
; P such that every identifier declared or defined in P 
; is bound in (decls env) or (defs env), respectively.
(define (decode env port)
  (match (read-solution port)
    [(? hash? sol) 
     (sat (for/hash ([(const id) (in-dict (decls env))])
            (values const 
                    (if (hash-has-key? sol id)
                        (decode-binding const (hash-ref sol id))
                        (default-binding const)))))]
    [#f (unsat)]))

; Given a solver input port, the reset procedure prints
; commands necessary to clear the solver's state to the
; given port.
(define (clear-solver port)
  (cmd [port]
    (reset)))

(define (default-binding const)
  (match (type-of const)
    [(== @boolean?) #f]
    [(== @number?) 0]
    [(? bitvector? t) (bv 0 t)]
    [(? enum? t) (vector-ref (enum-members t) 0)]))

(define (to-exact-int a) (if (integer? a) (inexact->exact a) a))

(define (decode-binding const val)
  (match (type-of const)
    [(== @boolean?)
     (match val
       [(== true) #t]
       [(== false) #f]
       [_ (error 'decode-binding "expected 'true or 'false binding for ~a, given ~a" const val)])]
    [(== @number?) 
     (match val
       [(? number?) (finitize val)]
       [(list _ (app symbol->string (regexp #px"bv(\\d+)" (list _ (app string->number n)))) _)
        (finitize n)])]
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
