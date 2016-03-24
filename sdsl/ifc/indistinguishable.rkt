#lang rosette

(require rosette/lib/match "machine.rkt")

(provide ≈ mem≈ low≈ full≈)
         
; See Def. 2.3.1 in the full draft of "Testing Noninterference, Quickly".
(define (≈ v0 v1)
  (match* (v0 v1)
    [((@ x Lx) (@ y Ly))
     (and (equal? Lx Ly)
          (or (equal? Lx ⊤)
              (equal? x y)))]
    [((R x _) (R y _)) (≈ x y)]
    [((instruction p0 args0) (instruction p1 args1))
     (and (equal? p0 p1)
          (≈ args0 args1))]
    [((? list? v0) (? list? v1))
     (and (= (length v0) (length v1))
          (andmap ≈ v0 v1))]
    [(_ _) #f]))

; See Def. 5.1.1. in the full draft of "Testing Noninterference, Quickly".
(define (mem≈ v0 v1)
  (match* (v0 v1)
    [((machine (@ _ L0) _ mem0 insts0) (machine (@ _ L1) _ mem1 insts1))
     (and (equal? L0 L1)
          (or (equal? L0 ⊤)
              (and (≈ insts0 insts1)
                   (≈ mem0 mem1))))]
    [(_ _) #f]))

; See Def. 6.1.1. in the full draft of "Testing Noninterference, Quickly".
(define (low≈ v0 v1)
  (match* (v0 v1)
    [((machine (@ pc0 L0) stack0 mem0 insts0) (machine (@ pc1 L1) stack1 mem1 insts1))
     (and (equal? L0 L1)
          (or (equal? L0 ⊤)
              (and (= pc0 pc1)
                   (≈ insts0 insts1)
                   (≈ stack0 stack1)
                   (≈ mem0 mem1))))]
    [(_ _) #f]))

; See Def. 6.4.2. in the full draft of "Testing Noninterference, Quickly".
(define (full≈ v0 v1)
  (match* (v0 v1)
    [((machine (@ pc0 L0) stack0 mem0 insts0) (machine (@ pc1 L1) stack1 mem1 insts1))
     (and (equal? L0 L1)
          (≈ insts0 insts1)
          (≈ mem0 mem1)
          (if (equal? L0 ⊥)
              (and (equal? pc0 pc1)         
                   (≈ stack0 stack1))
              (≈ (cropStack stack0) (cropStack stack1))))]))

; The cropStack helper function takes a stack and removes elements from 
; the top until it reaches the first low return address (or until all 
; elements are removed).
(define (cropStack s)
  (or (memf low-return-address? s) '()))

; Returns true iff v is a return value, and (Rpc v) has the low label.
(define (low-return-address? v)
  (and (return? v)
       (let@ ([(_ L) (Rpc v)])
             (equal? L ⊥))))
  
