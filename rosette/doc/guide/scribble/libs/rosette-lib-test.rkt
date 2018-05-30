#lang rosette

(require rosette/lib/synthax)

(define (div2 x) ([choose bvshl bvashr bvlshr bvadd bvsub bvmul] x (?? (bitvector 8))))
(define-symbolic i (bitvector 8))
(define m1
 (synthesize #:forall (list i)
             #:guarantee (assert (equal? (div2 i) (bvudiv i (bv 2 8))))))
(print-forms m1)
(generate-forms m1)

(define-synthax (nnf x y depth)
  #:base (choose x (! x) y (! y))
  #:else (choose
          x (! x) y (! y)
          ((choose && ||) (nnf x y (- depth 1))
                          (nnf x y (- depth 1)))))

(define (nnf=> x y) (nnf x y 1))

(define-symbolic a b boolean?)

(print-forms
  (synthesize
   #:forall (list a b)
   #:guarantee (assert (equal? (=> a b) (nnf=> a b)))))

; A grammar for linear arithmetic.
(define-synthax LA
 ([(_ e ...) (+ (* e (??)) ...)]))
 
; The following query has no solution because (??) in
; (LA e ...) generates a single integer hole that is
; shared by all e passed to LA, in this case x and y.
(define-symbolic* x y integer?)

(define sol
  (synthesize
   #:forall (list x y)
   #:guarantee (assert (= (LA x y) (+ (* 2 x) y)))))

sol

; The following query has a solution because the second
; clause of LA2 creates two independent (??) holes.
(define-synthax LA2
  ([(_ e) (* e (??))]
   [(_ e1 e2) (+ (* e1 (??)) (* e2 (??)))]))
 
(define sol2
  (synthesize
   #:forall (list x y)
   #:guarantee (assert (= (LA2 x y) (+ (* 2 x) y)))))

(print-forms sol2)

#|
(define-synthax [shift terminal ... k]
  #:assert (>= k 0)
  [choose
   terminal ... (??)
   ([choose >> << >>>] (shift terminal ... (- k 1))
                       (shift terminal ... (- k 1)))])

(define (div2mul4 x) (shift x 2))

(define m2
 (synthesize #:forall (list i)
             #:assume (assert (>= i 0))
             #:guarantee (assert (= (div2mul4 i) (* 4 (quotient i 2))))))
(print-forms m2)|#
