#lang racket
 
(require (for-syntax racket)
         "../util/array.rkt" "../core/term.rkt" "state.rkt")

(provide define-symbolic define-symbolic*)

#|--------------define forms--------------|#

(define-syntax (define-symbolic stx)
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

(define-syntax (define-symbolic* stx)
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

#|--------------helper functions--------------|#

(module util racket
  (require racket/syntax)
  (provide var-ids indices)
  
  (define (var-ids id-stx dim-spec [separator '@])
    (for/list ([idx (apply indices (dims dim-spec))]) 
      (format-id id-stx "~a~a~a" id-stx separator idx #:source id-stx)))
  
  (define (dims spec)
    (begin0 spec
            (for ([dim spec])
              (unless (and (integer? dim) (>= dim 0))
                (error 'define-symbolic "expected a non-negative integer, given ~a" dim)))))
  
  (define (indices . k) 
    (cond [(null? k) k]
          [(null? (cdr k)) (build-list (car k) (lambda (i) (format-symbol "~a" i)))]
          [else (let ([car-idx (indices (car k))]
                      [cdr-idx (apply indices (cdr k))])
                  (append-map (lambda (i) 
                                (map (lambda (j) 
                                       (format-symbol "~a:~a" i j)) 
                                     cdr-idx)) 
                              car-idx))])))

(require (for-syntax 'util) 'util)

(define-for-syntax (define-array stx var type dims)
  (with-syntax ([var var]
                [type type]
                [(k ...) dims])
    (with-handlers ([exn:fail? 
                     (lambda (e) 
                       (case (syntax-local-context)
                         [(module top-level) 
                          (quasisyntax/loc stx 
                            (define var (reshape (list k ...) 
                                                 (map (lambda (id) (constant id type)) 
                                                      (var-ids #'var (list k ...))))))]
                         [else (raise e)]))])
      (with-syntax ([(v ...) (var-ids #'var (eval #'(list k ...)))]) 
        (quasisyntax/loc stx 
          (define var (reshape (list k ...) (list (constant #'v type) ...))))))))
  
