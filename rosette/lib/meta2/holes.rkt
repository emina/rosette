#lang racket

(require "core.rkt" "form.rkt")

(provide solution->forms print-forms
         define-synthax ?? choose)

(define (print-forms sol)
  (for ([f (solution->forms sol)])
    (printf "~a:~a:~a\n" (syntax-source f) (syntax-line f) (syntax-column f))
    (printf "~a\n" (pretty-format (syntax->datum f)))))