#lang rosette

(require rosette/base/tracer/tracer)

(define-symbolic a b boolean?)

(if a (if b (1) (2)) 3)

(query-root)
