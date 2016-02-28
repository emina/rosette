#lang rosette

(require "machine.rkt")

(provide (all-defined-out))

; See Section 5 of the full draft of "Testing Noninterference, Quickly".
; The basic semantics is expressed using the next, goto, 
; peek, push, pop, pop-until, read and write primitives 
; for manipulating machine state.  These are defined in machine.rkt.
; The basic semantics (Halt Noop Push Pop Add Load Store) is defined in basic.rkt.

(define (Jump*AB m)
  (let@ ([(x _) (peek m 0)])
    (goto (pop m) (@ x ⊥))))

(define (Jump*B m)
  (let@ ([x (peek m 0)])
    (goto (pop m) x)))

(define (Jump m)
  (let@ ([(x Lx)  (peek m 0)]
         [(_ Lpc) (pc m)])
    (goto (pop m) (@ x (∨ Lx Lpc)))))


  

