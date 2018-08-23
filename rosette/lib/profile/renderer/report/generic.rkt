#lang racket

(require racket/generic)
(provide (all-defined-out))

(define-generics report-component
  (init-component report-component)
  (receive-data report-component profile))
