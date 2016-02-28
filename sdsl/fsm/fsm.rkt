#lang rosette

(require "automaton.rkt" "query.rkt")

(provide automaton ? reject viz 
         verify-automaton
         debug-automaton 
         solve-automaton 
         synthesize-automaton matches?
         (rename-out [define/debug define])
         #%app #%top #%top-interaction #%module-begin #%datum
         quote)
