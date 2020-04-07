#lang rosette

(define-symbolic b boolean?)

(if b (if (! b) (1) 2) 3)
