#lang racket
 
(require syntax/parse (for-syntax syntax/parse racket)
         "../core/term.rkt")

(provide define-symbolic define-symbolic*)

(define-for-syntax (module-or-top? . args)
  (case (syntax-local-context)
    [(module top-level) #t]
    [else #f]))

(define-for-syntax (static? k)
  (with-handlers ([exn:fail? module-or-top?])
    (natural? (eval k)))) 

(define-syntax (define-symbolic stx)
  (syntax-parse stx
    [(_ var:id type)
     #'(define var (constant #'var type))]
    [(_ var:id type #:length k)
     #:declare k (expr/c #'natural? #:name "length argument")
     #:fail-unless (static? #'k) "expected a natural? for #:length"
     #'(define var
         (for/list ([i k.c])
           (constant (list #'var i) type)))]
    [(_ var:id ...+ type)
     #'(begin (define-symbolic var type) ...)]))

(define current-index (make-parameter 0))

(define (index!)
  (define idx (current-index))
  (current-index (add1 idx))
  idx)

(define-syntax (define-symbolic* stx)
  (syntax-parse stx
    [(_ var:id type)
     #'(define var (constant (list #'var (index!)) type))]
    [(_ var:id type #:length k)
     #:declare k (expr/c #'natural? #:name "length argument")
     #'(define var
         (for/list ([i k.c])
           (define-symbolic* var type)
           var))]
    [(_ var:id ...+ type)
     #'(begin (define-symbolic* var type) ...)]))
