#lang rosette

(define-symbolic xs integer? #:length 4)
(define (sum xs) (foldl + xs)) ; bug: missing 0 after +
(define-symbolic opt boolean?)
(synthesize
 #:forall xs
 #:guarantee (assert (= (sum xs) (apply (if opt + -) xs))))
