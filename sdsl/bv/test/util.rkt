#lang racket

(require rackunit (only-in "../bv.rkt" synthesize-fragment))

(provide (all-defined-out))

(define-syntax-rule (test-fragment (id param ...) expr ...)
  (test-case 
   (symbol->string 'id)
   (check-not-exn 
    (lambda ()
      (synthesize-fragment (id param ...) expr ...))
    (format "Failed to synthesize ~a.\n" 'id))))
            