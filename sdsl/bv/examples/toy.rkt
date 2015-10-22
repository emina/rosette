#lang s-exp rosette

(require "../bv.rkt")

; This is designed to work with bitvectors.
; We'll use 1-bit bitvectors to simulate booleans.
; All numbers are signed, so 0=false, -1=true.
(current-bitwidth 1) 

; The spec accepts 4 inputs and their negations.
; All bv operators are binary/unary.
(define (spec a b c d !a !b !c !d)
  (bvor
   (bvor (bvand a !b) 
         (bvand !a b))
   (bvor (bvand c !d) 
         (bvand !c d))))

(define-fragment (circuit a b c d !a !b !c !d)
  #:requires ; precondition:  the last four inputs are negations of the first four inputs
  (lambda (a b c d !a !b !c !d) 
    (and (= !a (bvnot a)) (= !b (bvnot b)) (= !c (bvnot c)) (= !d (bvnot d))))
  #:ensures  ; postcondition: output of synthesized circuit matches the spec on the given inputs
  (lambda (a b c d !a !b !c !d out)
    (= out (spec a b c d !a !b !c !d)))
  #:library  ; synthesized circuit can use at most 3 or gate and 4 and gates
  (bvlib [{bvor} 3] [{bvand} 4]))

; Call this function show the synthesized code:
(define (show)
  (pretty-print (syntax->datum circuit-stx)))
  



