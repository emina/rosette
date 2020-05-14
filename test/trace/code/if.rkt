#lang rosette

(define-symbolic a b boolean?)
(if b (1) #f)
(if a (if b (1) (2)) 3)
