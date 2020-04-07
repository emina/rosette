#lang rosette

(define-symbolic xs integer? [4])
(define-symbolic k integer?)
(define (sum-buggy xs) (foldl + xs))
(verify
 #:assume (assert (= k (sum-buggy xs)))
 #:guarantee (assert (= k (sum-buggy (filter (compose not zero?) xs)))))
