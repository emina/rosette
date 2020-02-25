#lang rosette/safe

(require rosette/query/debug rosette/lib/render) 
 
(define (poly x)
 (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))
 
(define/debug (factored x)
 (* x (+ x 1) (+ x 2) (+ x 2)))
 
(define (same p f x)
 (assert (= (p x) (f x))))

(define-symbolic i integer?)

(define cex (verify (same poly factored i)))

(evaluate i cex)

(define c (debug [integer?] (same poly factored -6)))
(render c)

(require rosette/lib/synthax)

(define (factored* x)        
  (* (+ x (??)) (+ x 1) (+ x (??)) (+ x (??))))  

(define binding 
  (synthesize #:forall (list i)
              #:guarantee (same poly factored* i)))

(print-forms binding)

(define-symbolic x y integer?)
(define env 
  (solve (begin (assert (not (= x y)))
                (assert (< (abs x) 10))
                (assert (< (abs y) 10))
                (assert (not (= (poly x) 0)))
                (assert (= (poly x) (poly y))))))
env
