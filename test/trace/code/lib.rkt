#lang racket

(require rosette)
(provide mac)
(define-syntax-rule (mac)
  (begin
    (begin
      (define foo (verify (1)))
      (define bar 1))))
