#lang rosette

(require "machine.rkt" )

(provide (all-defined-out))
; See Section 5 of the full draft of "Testing Noninterference, Quickly".
; The basic semantics is expressed using the next, goto, 
; peek, push, pop, pop-until, read and write primitives 
; for manipulating machine state.  These are defined in machine.rkt.
; The basic semantics is defined in basic.rkt.
;
; To make instruction implementation uniform, we use values rather 
; than integers for Call and Return arguments; the label of the value 
; is simply ignored.

(define (Call*B m k)
  (let@ ([(k Lk)   k]
         [(x Lx)   (peek m 0)]
         [(pc Lpc) (pc m)])
    (assert (equal? Lk ⊥))  
    (goto (push (pop m) k (R (@ (add1 pc) Lpc))) (@ x (∨ Lx Lpc)))))

(define (Return*AB m n)
  (let@ ([(n Ln) n]
         [v (peek m 0)]
         [m (pop-until m return?)]
         [r (peek m 0)])
    (assert (equal? Ln ⊥))
    (assert (|| (= 0 n) (= 1 n)))
    (if (= n 0)
        (goto (pop m) (Rpc r))
        (goto (push (pop m) v) (Rpc r)))))

(define (Return*B m n)
  (let@ ([(n Ln)  n]
         [(_ Lpc) (pc m)]
         [(v _) (peek m 0)]
         [m (pop-until m return?)]
         [r (peek m 0)])
    (assert (equal? Ln ⊥))
    (assert (|| (= 0 n) (= 1 n)))
    (if (= n 0)
        (goto (pop m) (Rpc r))
        (goto (push (pop m) (@ v Lpc)) (Rpc r))))) ; bug (see Return)

; We use StoreCR for the Store rule that is correct in 
; the presence of Call/Return to avoid conflict with the 
; basic Store rule.
(define (StoreCR m)
  (let@ ([(x Lx)  (peek m 0)]
         [(y Ly)  (peek m 1)]
         [(_ Lpc) (pc m)]
         [(_ Lmx) (read m x)])
    (assert (⊑ (∨ Lpc Lx) Lmx) "no sensitive upgrade")
    (next (write (pop m 2) x (@ y (∨ Lx Ly Lpc))))))

(define (PopCR m)
  (assert (value? (peek m)))
  (next (pop m)))

(define (Call m k n)
  (let@ ([(k Lk)   k]
         [(vn Ln)  n]
         [(x Lx)   (peek m 0)]
         [(pc Lpc) (pc m)])
    (assert (equal? Lk ⊥))
    (assert (equal? Ln ⊥))
    (assert (|| (= 0 vn) (= 1 vn)))
    (goto (push (pop m) k (R (@ (add1 pc) Lpc) n)) (@ x (∨ Lx Lpc)))))

; Bug in the paper: the label on the returned value should be joined 
; with the PC label (it cannot be just the PC label).
(define (Return m)
  (let@ ([(v Lv)  (peek m 0)]
         [m       (pop-until m return?)]
         [r       (peek m 0)]
         [(n _)   (Rn r)]
         [(_ Lpc) (pc m)])
    (if (= n 0) 
        (goto (pop m) (Rpc r))
        (goto (push (pop m) (@ v (∨ Lv Lpc))) (Rpc r)))))

                   

#|
(define p0
  (vector-immutable
   (instruction Push (@ 3 ⊤))
   (instruction Call*B 0@⊥)
   (instruction Halt)
   (instruction Push (@ 1 ⊥))
   (instruction Push 0@⊥)
   (instruction Store)
   (instruction Return*AB 0@⊥)))

(define p1
  (vector-immutable
   (instruction Push (@ 6 ⊤))
   (instruction Call*B 0@⊥)
   (instruction Halt)
   (instruction Push (@ 1 ⊥))
   (instruction Push 0@⊥)
   (instruction Store)
   (instruction Return*AB 0@⊥)))

(define m0 (init p0))
(define m1 (init p1))

(define m0k (step m0 7))
(define m1k (step m1 7))

m0k
m1k|#

#|

(define (Call*B m n)
  (let@ ([(n Ln)    n]
         [(x Lx)   (peek m 0)]
         [(pc Lpc) (pc m)])
    (assert (equal? Ln ⊥)) ; ignore Ln
    (goto (push (pop m) n (R (@ (add1 pc) Lpc))) (@ x (∨ Lx Lpc)))))

(define (Return*AB m n)
  (cond [(= (@int n) 0) 
         (let@ ([m (pop-until m return?)]
                [r (peek m 0)])
           (goto (pop m) (Rpc r)))]
        [else 
         (assert (= (@int n) 1))
         (let@ ([v (peek m 0)]
                [m (pop-until (pop m) return?)]
                [r (peek m 0)])
           (goto (push (pop m) v) (Rpc r)))]))


(define (Return*B m n)
  (cond [(= (@int n) 0) 
         (let@ ([m (pop-until m return?)]
                [r (peek m 0)])
           (goto (pop m) (Rpc r)))]
        [else 
         (assert (= (@int n) 1))
         (let@ ([(v Lv) (peek m 0)]
                [m (pop-until (pop m) return?)]
                [r (peek m 0)]
                [(_ Lpc) (pc m)])
           (goto (push (pop m) (@ v Lpc)) (Rpc r)))]))

(define (Call m n v)
  (assert (|| (= 0 (@int v)) (= 1 (@int v))))
  (assert (equal? ⊥ (@label v)))
  (assert (equal? ⊥ (@label n)))
  (let@ ([(n _)    n]
         [(x Lx)   (peek m 0)]
         [(pc Lpc) (pc m)])
    (goto (push (pop m) n (R (@ (add1 pc) Lpc) v)) (@ x (∨ Lx Lpc)))))

; Bug in the paper: the label on the returned value should be joined 
; with the PC label (it cannot be just the PC label).
(define (Return m)
  (let@ ([(v Lv)  (peek m 0)]
         [m       (pop-until m return?)]
         [r       (peek m 0)]
         [(n _)   (Rn r)]
         [(_ Lpc) (pc m)])
    (if (= n 0) 
        (goto (pop m) (Rpc r))
        (goto (push (pop m) (@ v (∨ Lv Lpc))) (Rpc r)))))

|#
