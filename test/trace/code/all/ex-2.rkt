#lang rosette

(define (len-buggy ys)
  (cond
    [(empty? ys) 0]
    [else (add1 (len-buggy (first ys)))]))

(define-symbolic xs integer? [4])
(define-symbolic k integer?)
(define-symbolic n integer?)
(define ys (take xs n))

(verify
 #:assume (assert (= k (len-buggy ys)))
 #:guarantee (assert (= k (len-buggy (map add1 ys)))))
