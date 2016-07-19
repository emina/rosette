#lang racket

(require (for-syntax (only-in racket/syntax format-id))
         (only-in racket/generic define-generics)
         (only-in "../form/control.rkt" @if)
         (only-in "../core/bool.rkt" @assert)
         (only-in "../core/forall.rkt" for/all)
         "../core/union.rkt")

(provide @define-generics @make-struct-type-property)

(define-syntax (@define-generics stx)
  (syntax-case stx ()
    [(_ id method ...)
     (with-syntax ([id? (format-id #'id "~a?" #'id #:source #'id)])
       (syntax/loc stx 
         (begin
           (define-generics id
            method ...)
           (set! id? (lift id? receiver))
           (handle-method method) ...)))]))

(define-syntax (handle-method stx)
  (syntax-case stx ()
    [(_ (method self arg ...))
     (syntax/loc stx
       (set! method (lift method receiver arg ...)))]
    [(_ (method self arg ... . rest))
     (syntax/loc stx
       (set! method (lift-variadic method receiver arg ... . rest)))]))
    

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

(define-syntax-rule (lift-variadic proc receiver arg ... . rest)
  (let ([proc proc])
    (procedure-rename
     (lambda (receiver arg ... . rest)
      (if (union? receiver)
          (for/all ([r receiver]) (apply proc r arg ... rest))
          (apply proc receiver arg ... rest)))
     (or (object-name proc) 'proc))))


#|
; sanity check
(@define-generics foo [some foo])
some

(struct bar (arg)
  #:methods gen:foo
  [(define (some self) (bar-arg self))])

(some (bar 'yes))

(require (only-in rosette/base/form/define define-symbolic)
         (only-in rosette/base/core/bool @boolean?)
         (only-in rosette/base/core/real @* @+))

(define-symbolic b @boolean?)
(some (@if b (bar 'yes) (bar 'no)))
(foo? (@if b (bar 'yes) (bar 'no)))

(@define-generics xyzzy
  (h xyzzy))

(@define-generics variadic
  (f variadic . x)
  (g variadic))

(struct multiplier (y) #:transparent
  #:methods gen:variadic
  [(define (f self . x) (foldl @* (multiplier-y self) x))
   (define (g self) 'g-mult)]
  #:methods gen:xyzzy
  [(define (h self) 42)])

(struct adder (z) #:transparent
  #:methods gen:variadic
  [(define (f self . x) (foldl @+ (adder-z self) x))
   (define (g self) 'g-add)])

(define thing (@if b (multiplier 3) (adder 3)))
(variadic? thing)
thing
(f thing 2 5)
(g thing)
(h thing)|#
