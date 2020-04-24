#lang rosette

(define-symbolic xs integer? [4])
(define (sum xs)
  (cond
    [(null? xs) 0]
    [(null? (cdr xs)) (car xs)]
    [(andmap (curry = (car xs)) (cdr xs))
     (* (length xs) (cdr xs))] ; bug: cdr should be car
    [else (apply + xs)]))

(verify
 #:assume (assert (positive? (sum xs)))
 #:guarantee (assert (ormap positive? xs)))
