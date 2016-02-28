#lang rosette

(require "viz.rkt" (rename-in rosette/lib/synthax [choose ?]))

(provide automaton reject ? alphabet viz)

; Taken from Shriram Krishnamurthi, "Automata via Macros," Journal 
; of Functional Programming, 2006. We correct the definition 
; so that only states with no outgoing edges are accepting.  We also 
; wrap the automaton procedure into an fsm value that also keeps the 
; machine's graph structure (for display purposes).

(define-syntax automaton
  (syntax-rules (: →)  
    [(_ init-state
        (state : (label → target) ...) ...)
     (letrec ([state
               (lambda (stream)
                 (cond
                   [(empty? stream) (empty? '(label ...))]  
                   [else
                    (case (first stream)
                      [(label) (target (rest stream))] ...
                      [else false])]))]
              ...) 
       (fsm '((state (label target) ...) ...) init-state))]))

; A special state that rejects all labels.
(define reject (lambda (stream) false))

; An FSM consists of a graph representation and an executable implementation.
(struct fsm (graph exec)
  #:property prop:procedure
  (struct-field-index exec))

; Returns the alphabet of the given automaton.
(define (alphabet m)
  (remove-duplicates 
   (for/fold ([out '()]) ([ne (fsm-graph m)] #:unless (null? (cdr ne)))
     (append out (map car (cdr ne))))))
         
; Produces a bitmap drawing of the given FSM's graph.
(define (viz m) (automaton->bitmap (fsm-graph m)))
