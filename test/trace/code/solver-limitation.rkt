#lang rosette

(define-symbolic b boolean?)
(assert b)
(if b 1 (error 'bad))
