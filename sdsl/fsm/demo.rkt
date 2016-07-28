#lang rosette

(require "fsm.rkt")

(define m 
  (automaton init 
   [init : (c → more)]
   [more : (a → more) 
           (d → more) 
           (r → end)] 
   [end : ]))

(define rx #px"^c[ad]+r$")


(define M 
  (automaton init
   [init : (c → (? s1 s2))]
   [s1   : (a → (? s1 s2 end reject)) 
           (d → (? s1 s2 end reject))
           (r → (? s1 s2 end reject))]
   [s2   : (a → (? s1 s2 end reject)) 
           (d → (? s1 s2 end reject))
           (r → (? s1 s2 end reject))]
   [end  : ]))

; example commands 
(m '(c a r)) 
(m '(c d r))
(m '(c a d a r))
(verify-automaton m #px"^c[ad]+r$")
(debug-automaton m #px"^c[ad]+r$" '(c r))
(synthesize-automaton M #px"^c[ad]+r$")
