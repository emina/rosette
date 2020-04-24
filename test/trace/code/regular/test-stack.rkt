#lang rosette

(define-symbolic b boolean?)
(define (f) (error 'bad))
(if b (f) 1)
