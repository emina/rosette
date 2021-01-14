#lang rosette

(define-symbolic xs integer? #:length 4)
(define (sum xs) (foldl + xs)) ; bug: missing 0 after +
(verify
 #:assume (assert (positive? (sum xs)))
 #:guarantee (assert (ormap positive? xs)))
