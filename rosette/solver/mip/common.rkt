#lang racket

(provide (all-defined-out))

(struct objective (type expr))

(define (get-name v)
  (define name (format "~a" v))
  (string-replace name "$" ""))