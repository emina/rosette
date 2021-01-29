#lang rosette

(define-symbolic xs integer? #:length 4)
(define (sum xs)
  (cond
    [(null? xs) 0]
    [(null? (cdr xs)) (car xs)]
    [(andmap (curry = (car xs)) (cdr xs))
     (* (length xs) (cdr xs))] ; bug: cdr should be car
    [else (apply + xs)]))

(verify
 (begin
   (assume (positive? (sum xs)))
   (assert (ormap positive? xs))))
