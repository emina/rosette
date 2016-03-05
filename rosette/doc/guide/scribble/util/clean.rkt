#lang racket

(define (strip xs)
  (printf "'(")
  (for ([x xs])
    (printf "~a " x))
  (printf ")"))

