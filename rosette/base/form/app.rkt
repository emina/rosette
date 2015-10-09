#lang racket

(require "../core/assert.rkt")

(provide (rename-out [app #%app] [app #%plain-app]))

(define-syntax (app stx)
  (syntax-case stx ()
    [(app proc arg ...) 
     (quasisyntax/loc stx (relax (#%app proc arg ...) #,#'proc))]))
     
