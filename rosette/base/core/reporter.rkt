#lang racket

(require racket/generic (for-syntax racket/syntax))
(provide (all-defined-out))

; The reporter is called when "interesting"
; events happen during symbolic execution; for example,
; when a merge occurs or a new term is created.
(define current-reporter
  (make-parameter
    void
    (lambda (new-reporter)
      (unless (procedure? new-reporter)
        (raise-argument-error 'current-reporder "procedure?" new-reporter))
      new-reporter)))
