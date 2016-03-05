#lang rosette/safe

;(configure [bitwidth 8])

(require rosette/query/debug rosette/lib/tools/render) 
 
(define (poly x)
 (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))
 
(define/debug (factored x)
 (* x (+ x 1) (+ x 2) (+ x 2)))
 
(define (same p f x)
 (assert (= (p x) (f x))))

(define-symbolic i number?)

(define cex (verify (same poly factored i)))

(evaluate i cex)

(define core (debug [number?] (same poly factored 4)))
(render core)

(require rosette/lib/meta/meta)

(define (factored* x)        
  (* (+ x (??)) (+ x 1) (+ x (??)) (+ x (??))))  

(define binding 
  (synthesize #:forall (list i)
              #:guarantee (same poly factored* i)))

(print-forms binding)

(define-symbolic x y number?)
(define env 
  (solve (begin (assert (not (= x y)))
                (assert (< (abs x) 10))
                (assert (< (abs y) 10))
                (assert (not (= (poly x) 0)))
                (assert (= (poly x) (poly y))))))
env
