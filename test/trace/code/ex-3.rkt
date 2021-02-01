#lang rosette

(define-symbolic xs integer? #:length 4)
(define-symbolic k integer?)
(define-symbolic n integer?)
(define (select xs n)
  (cond
    [(empty? xs) (assert #f "unexpected empty list")]
    [else (define pivot (first xs))
          (define non-pivot (rest xs))
          (define <pivot (filter (λ (x) (< x pivot)) non-pivot))
          (define >=pivot (filter (λ (x) (>= x pivot)) non-pivot))
          (define len< (length <pivot))
          (cond
            [(= n len<) pivot]
            [(< n len<) (select <pivot)]
            [else (select >=pivot (- n len< 1))])]))

(verify
 (begin
   (assume (and (<= 0 n (sub1 (length xs)))
                (= k (select xs n))))
   (assert (= k (list-ref (sort xs <) n)))))
