#lang racket

(require racket/syntax)

(require "../../base/term.rkt" "../../base/num.rkt" "../solution.rkt"
         (only-in "kks.rkt" cmd configure declare-univ declare-ints assert solve read-solution)
         "env.rkt" "enc.rkt" "univ.rkt")

(provide encode decode)

; Given an output port, an encoding environment, a list of 
; asserts, and an optional list of keyword-value configuration 
; settings, the encode procedure prints a Kodkod encoding of the 
; given assertions and options to the given port, with respect to 
; the given environment. In particular, the encoder will not emit 
; any declarations or definitions for Rosette values that appear in 
; the given assertions and that are already bound in the environment.  
; The environment will be augmented, if needed, with additional 
; declarations and definitions.
(define (encode port env asserts . opts)
  (cmd [port]
       (when (pair? opts)                   ; some options given
         (apply configure opts))
       (when (= (dict-count (defs env)) 0)  ; universe not already declared
         (let* ([univ (univ env)]
                [ints (domain-of univ @number?)])
           (declare-univ (universe-size univ))
           (declare-ints (domain-values ints) (domain-indices ints))))
       (for ([a asserts])
         (assert (enc a env)))
       (solve)))

; Given a solver input port and an encoding enviornment,
; the decode procedure reads the solution from the port 
; and converts it into a Rosette solution object.  The 
; port must be connected to a solver working on a problem 
; P such that every identifier declared or defined in P 
; is bound in (decls env) or (defs env), respectively.
(define (decode port env)
  (match (read-solution port)
    [(? hash? sol)  ; sol is a map from relation ids to relations (lists of lists of integers)
     (let ([univ (univ env)])
       (sat (for/hash ([(val rel) (in-dict (decls env))] 
                       #:when (and (term? val) (hash-has-key? sol rel)))
              (values val (domain-value (domain-of univ (type-of val))
                                        (hash-ref sol rel))))))]
    [(list) (unsat)] ; no core extracted
    [(app (curry apply set) core)  ; core extracted
     (unsat (for/list ([(key id) (in-dict (defs env))]
                       #:when (set-member? core id))
              key))]))



