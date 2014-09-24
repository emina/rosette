#lang s-exp rosette
(require (for-syntax racket/syntax))

(provide (all-defined-out))

(define-syntax (define-const stx)
  (syntax-case stx ()
    [(_ val) #`(define #,(format-id #'val "const~a" (syntax-e #'val) #:source #'val) (lambda () val))]
    [(_ v0 v ...) #`(begin (define-const v0) (define-const v) ...)]))

(define-const 0 1 31)
(define (const-max-shift) (- (current-bitwidth) 1))

