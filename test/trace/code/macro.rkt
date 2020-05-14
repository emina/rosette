#lang rosette

(define-symbolic a b boolean?)
(cond
  [a (1)]
  [b 2]
  [else (3)])

(define-syntax-rule (foo)
  (1))

(if b (foo) #f)
