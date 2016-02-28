#lang rosette

(require "machine.rkt")
(provide (all-defined-out))

; See Section 2 of the full draft of "Testing Noninterference, Quickly".
; The basic semantics is expressed using the next, peek, push, pop, read and write 
; primitives for manipulating machine state.  These are defined in machine.rkt.

(define (Halt m) 
  (let@ ([(_ Lpc) (pc m)])
    (goto m (@ (LOC m) Lpc))))

(define (Noop m)   (next m))
(define (Push m v) (next (push m v)))
(define (Pop m)    (next (pop m)))

(define (Load* m)
  (let@ ([(x Lx) (peek m 0)]
         [v      (read m x)])
    (next (push (pop m) v))))

(define (Store*AB m)
  (let@ ([(x _) (peek m 0)]
         [y     (peek m 1)])
    (next (write (pop m 2) x y))))

(define (Store*B m)
  (let@ ([(x Lx) (peek m 0)]
         [(y Ly) (peek m 1)])
    (next (write (pop m 2) x (@ y (∨ Lx Ly))))))

(define (Add* m)
  (let@ ([(x _) (peek m 0)]
         [(y _) (peek m 1)])
    (next (push (pop m 2) (@ (+ x y) ⊥)))))

(define (Load m)
  (let@ ([(x Lx) (peek m 0)]
         [(v Lv) (read m x)])
    (next (push (pop m) (@ v (∨ Lx Lv))))))

(define (Store m)
  (let@ ([(x Lx)  (peek m 0)]
         [(y Ly)  (peek m 1)]
         [(_ Lmx) (read m x)])
    (assert (⊑ Lx Lmx) "no sensitive upgrade")
    (next (write (pop m 2) x (@ y (∨ Lx Ly))))))

(define (Add m)
  (let@ ([(x Lx) (peek m 0)]
         [(y Ly) (peek m 1)])
    (next (push (pop m 2) (@ (+ x y) (∨ Lx Ly))))))

