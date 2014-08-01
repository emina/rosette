#lang racket

(require (for-syntax (only-in racket/syntax format-id))
         (only-in racket/generic define-generics)
         (only-in "control.rkt" @if)
         (only-in "assert.rkt" @assert)
         (only-in "forall.rkt" for/all)
         "union.rkt")

(provide @define-generics @make-struct-type-property)

(define-syntax (@define-generics stx)
  (syntax-case stx ()
    [(_ id [method self arg ...] ...)
     (with-syntax ([id? (format-id #'id "~a?" #'id #:source #'id)])
       (syntax/loc stx 
         (begin
           (define-generics id
            [method self arg ...] ...)
           (set! id? (lift id? receiver))
           (set! method (lift method receiver arg ...)) ...)))]))

(define (@make-struct-type-property name [guard #f] [supers null] [can-impersonate? #f])
  (define-values (prop:p p? p-ref) 
    (make-struct-type-property name guard supers can-impersonate?))
  (values prop:p (lift p? self) (lift p-ref self)))

(define-syntax-rule (lift proc receiver arg ...)
  (let ([proc proc])
    (procedure-rename
     (lambda (receiver arg ...)
      (if (union? receiver)
          (for/all ([r receiver]) (proc r arg ...))
          (proc receiver arg ...)))
     (or (object-name proc) 'proc))))

#|
; sanity check
(define-generics foo [some foo])
some

(struct bar (arg)
  #:methods gen:foo
  [(define (some self) (bar-arg self))])

(some (bar 'yes))

(require (only-in rosette/base/define define-symbolic)
         (only-in rosette/base/bool @boolean?))

(define-symbolic b @boolean?)

(some (@if b (bar 'yes) (bar 'no)))
(foo? (@if b (bar 'yes) (bar 'no)))|#


    
