#lang rosette/safe

(define (poly x)
  (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))

(define (same-as-poly other x)
  (assert (= (poly x) (other x))))

(define (factored x)
  (* x (+ x 1) (+ x 2) (+ x 2)))

(define-symbolic n number?)
(define cex (time (verify (same-as-poly factored n))))
(evaluate n cex)

(require rosette/query/debug rosette/lib/tools/render)

(define/debug (factored-buggy x)
  (* x (+ x 1) (+ x 2) (+ x 2)))

(define core (time (debug [number?] (same-as-poly factored-buggy 4))))
(render core)

(require rosette/lib/meta/meta)

(define (factored-sketch x)
  (* (+ x (??)) (+ x 1) (+ x (??)) (+ x (??))))

(define sol (time (synthesize #:forall (list n)
                              #:guarantee (same-as-poly factored-sketch n))))

(print-forms sol)
  
