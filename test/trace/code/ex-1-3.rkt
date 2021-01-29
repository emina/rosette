#lang rosette

(define-symbolic xs integer? #:length 4)
(define (sum xs) (foldl + xs)) ; bug: missing 0 after +
(verify
 (begin
   (assume (positive? (sum xs)))
   (assert (ormap positive? xs))))
