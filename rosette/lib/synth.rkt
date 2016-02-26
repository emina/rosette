#lang racket

(require "synth/core.rkt" "synth/form.rkt" "util/syntax-properties.rkt")

(provide solution->forms print-forms
         define-synthax ?? choose
          (for-syntax save-properties) restore-properties)

(define (print-forms sol)
  (for ([f (solution->forms sol)])
    (printf "~a:~a:~a\n" (syntax-source f) (syntax-line f) (syntax-column f))
    (printf "~a\n" (pretty-format (syntax->datum f)))))