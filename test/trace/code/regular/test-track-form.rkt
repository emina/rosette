#lang rosette

(define-symbolic b boolean?)
(if b (let () (error 'bad)) 1)
