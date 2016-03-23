#lang rosette

(require rosette/lib/synthax)

(define (div2 x) ([choose bvshl bvashr bvlshr bvadd bvsub bvmul] x (?? (bitvector 8))))
(define-symbolic i (bitvector 8))
(define m1
 (synthesize #:forall (list i)
             #:guarantee (assert (equal? (div2 i) (bvsdiv i (bv 2 8))))))
(print-forms m1)
(generate-forms m1)

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
