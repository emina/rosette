#lang racket

(require "type.rkt")

(provide @any?)

; Universal type that accepts all Racket and Rosette values.  The subtype?
; method of every type must return #t when given univ as the argument.
(define-type @any? [any/c]
  #:pred any/c
  #:least-common-supertype (lambda (t) @any?)
  #:eq? eq?
  #:equal? equal?
  #:cast (lambda (v) (values #t v)))




  
