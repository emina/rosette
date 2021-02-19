#lang rosette

(require rosette/lib/match "value.rkt" "instruction.rkt")

(provide machine machine? halted? halted∩low? init all
         LOC step push peek pop pop-until read write goto next 
         (rename-out [machine-pc pc]
                     [machine-insts insts]
                     [machine-stack stack]
                     [machine-mem memory])
         (all-from-out "value.rkt" "instruction.rkt"))
     
; A machine consists of a program counter (represented as a value), a 
; stack (represented as a list of values), a memory (represented as an 
; list of values), and a program (represented as a list of instructions). 
(struct machine (pc stack mem insts) 
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "(machine [pc = ~s]\n"    (machine-pc self))
     (fprintf port "         [stack = ~s]\n" (machine-stack self))
     (fprintf port "         [mem = ~s]\n"   (machine-mem self))
     (fprintf port "         [insts = ~s]"   (machine-insts self)))])

; Returns a fresh machine initialized with the given program, a pc of 
; 0@⊥, an empty stack, and a memory with M cells set to 0@⊥.  The default 
; value for M is 2. 
; (-> (listof instruction?) machine?)
; (-> (listof instruction?) integer? machine?)
(define (init program [M 2])
  (machine 0@⊥ '() (make-list M 0@⊥) program))

; Returns a fresh machine initialized with the given program, an arbitrary
; pc, an arbitrary stack of depth S, and an arbitrary memory with M cells. 
; (-> (listof instruction?) integer? integer? machine?)
(define (all program S M)
  (machine (@ (?int) (?label)) 
           (for/list ([i S]) (if (?bool) 
                                 (@ (?int) (?label))
                                 (R (@ (?int) (?label)) (@ (?int) (?label)))))
           (for/list ([i M]) (@ (?int) (?label)))
           program))

; Returns the number of instructions comprising the machine's program.
(define (LOC m) 
  (length (machine-insts m)))

; Returns true iff the given machine is halted.
(define (halted? m)
  (let@ ([(pc _) (machine-pc m)])
    (= pc (LOC m))))

; Returns true iff the given machine is halted with the pc's label set to ⊥.
(define (halted∩low? m)
  (let@ ([(pc Lpc) (machine-pc m)])
    (and (= pc (LOC m)) (equal? ⊥ Lpc))))

; Returns the ith value from the top of the machine's stack, without removing it,
; where the default value for i is 0.
; (-> machine? value?)
; (-> machine? integer? value?)
(define (peek m [i 0]) 
  (list-ref (machine-stack m) i))

; Functionally inserts the given value at the ith slot in the machine's stack, 
; where the default value for i is 0.
; (-> machine? value? machine?)
; (-> machine? integer? value? machine?)
(define push
  (case-lambda 
    [(m v)   (struct-copy machine m [stack (cons v (machine-stack m))])]
    [(m i v) (struct-copy machine m [stack (insert (machine-stack m) i v)])]))

; Functionally removes k values from the top of the machine's stack, where 
; the default value for k is 1.
; (-> machine? integer? machine?)
; (-> machine? machine?)
(define (pop m [k 1])
  (struct-copy machine m [stack (drop (machine-stack m) k)]))

; Functionally removes values from the top of the machine's stack until it 
; encounters a value that satisfies the given predicate.  The stack of the 
; returned machine consists of that value and the rest of m's stack.
; (-> machine? (-> any/c boolean?) machine?)
(define (pop-until m pred)
  (struct-copy machine m [stack (memf pred (machine-stack m))]))

; Returns the value at the given address in the machine's memory.
; (-> machine? integer? value?)
(define (read m idx)    
  (list-ref (machine-mem m) idx))

; Functionally stores the given value at the given index in the machine's memory.
; (-> machine? integer? value? machine?)
(define (write m idx v) 
  (struct-copy machine m [mem (list-set (machine-mem m) idx v)]))

; Functionally sets the machine's pc to the given value. 
; (-> machine? value? machine?)
(define (goto m n) (struct-copy machine m [pc n]))

; Functionally advances the value of the machine's pc, leaving its tag unchanged.
; (-> machine? machine?)
(define (next m)
  (let@ ([(pc Lpc) (machine-pc m)])
    (struct-copy machine m [pc (@ (add1 pc) Lpc)])))

; Executes k steps of the machine, returning the resulting machine, where the 
; default value for k is 1.
; (-> machine? machine?)
; (-> machine? integer? machine?)
(define (step m [k 1])
  ;(printf "step ~a, stack ~a\n" k (machine-stack m))
  (cond [(or (= k 0) (halted? m)) m]
        [else (step 
               (match m
                 [(machine (@ i _) _ _ insts)
                  ((list-ref insts i) m)])
               (sub1 k))]))



  

