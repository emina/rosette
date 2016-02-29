#lang racket

(provide (all-defined-out))

(define verbose? (make-parameter #t))

(define (bv-info str . args)
  (when (verbose?)
    (apply printf str args)
    (newline)))
