#lang racket

(require racket/generic)
(provide (all-defined-out))

(define-generics renderer
  (start-renderer renderer profile reporter)
  (finish-renderer renderer profile))
