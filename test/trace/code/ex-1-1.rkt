#lang rosette

(define-symbolic xs integer? #:length 4)
(define (sum xs) (foldl + xs)) ; bug: missing 0 after +
(verify (assert (= (sum xs) (sum (filter-not zero? xs)))))
