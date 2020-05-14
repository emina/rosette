#lang rosette

(define-symbolic b boolean?)

(for/all ([x (if b '() '(1))])
  (rest x))
