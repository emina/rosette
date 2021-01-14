#lang racket
 
(require syntax/parse (for-syntax syntax/parse racket)
         "../core/term.rkt" "state.rkt")

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

(define-syntax (define-symbolic* stx)
  (syntax-parse stx
    [(_ [var:id oracle] type)
     #'(define var (constant (list #'var (oracle #'var)) type))]
    [(_ var:id type)
     #'(define-symbolic* [var (current-oracle)] type)]
    [(_ var:id type #:length k)
     #:declare k (expr/c #'natural? #:name "length argument")
     #'(define var
         (for/list ([i k.c])
           (define-symbolic* var type)
           var))]
    [(_ var:id ...+ type)
     #'(begin (define-symbolic* var type) ...)]))

#;(define-syntax (define-symbolic stx)
  (syntax-case stx ()
    [(_ var type)
     (identifier? #'var)
     (syntax/loc stx (define var (constant #'var type)))]
    [(_ var type [ k ... ])
     (and (identifier? #'var) (implies (identifier? #'type) (identifier-binding #'type)))
     (define-array stx #'var #'type #'(k ...))]
    [(_ v ... type)
     (andmap identifier? (syntax->list #'(v ...)))
     (syntax/loc stx (define-values (v ...) (values (constant #'v type) ...)))]))

#;(define-syntax (define-symbolic* stx)
  (syntax-case stx ()
    [(_ [var oracle] type)
     (identifier? #'var)
     (syntax/loc stx (define var (constant (list #'var (oracle #'var)) type)))]
    [(_ var type)
     (identifier? #'var)
     (syntax/loc stx (define-symbolic* [var (current-oracle)] type))]
    [(_ var type [ k ... ])
     (and (identifier? #'var) (implies (identifier? #'type) (identifier-binding #'type)))
     (syntax/loc stx (define var (reshape (list k ...) (for/list ([i (in-range (* k ...))])
                                                         (define-symbolic* var type)
                                                         var))))]
    [(_ v0 v ... type)
     (and (identifier? #'v0) (andmap identifier? (syntax->list #'(v ...))))
     (syntax/loc stx (begin (define-symbolic* v0 type) (define-symbolic* v type) ...))]
    ))
 
