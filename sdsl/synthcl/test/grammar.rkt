#lang s-exp "../lang/main.rkt"


(grammar int (expr [int x] [int y] [int depth])
  #:base (choose x y)
  #:else (locally-scoped
          (: int left right)
          (= left  (expr x y (- depth 1)))
          (= right (expr x y (- depth 1)))
          (+ left right)))

(procedure int (h0 [int x] [int y])
  (expr x y 1))

(procedure int (s0 [int x] [int y])
  (+ x y))

(synth #:forall [(: int x) (: int y)]
       #:ensure (assert (== (h0 x y) (s0 x y))))

