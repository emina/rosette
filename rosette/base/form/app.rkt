#lang racket

(require (for-syntax racket/syntax)
         racket/stxparam racket/stxparam-exptime)
         
(provide app (rename-out [app #%app] [app #%plain-app]))

(define-syntax-parameter app
  (syntax-rules ()
    [(_ proc arg ...) (#%app proc arg ...)]))

