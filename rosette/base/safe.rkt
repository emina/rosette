#lang racket

(require "assert.rkt"
         "type.rkt"
         "bool.rkt"
         racket/performance-hint)

(provide argument-error arguments-error type-error contract-error index-too-large-error
         assert coerce assert-some assert-|| assert-bound assert-arity-includes)

(begin-encourage-inline
  
  (define (arguments-error name message . field-value)
    (thunk (apply raise-arguments-error name message field-value)))
  
  (define (argument-error name expected given)
    (thunk (raise-argument-error name expected given)))
  
  (define (type-error name expected given)
    (argument-error name (format "~a" expected) given))  
  
  (define (contract-error name contract given)
    (argument-error name (format "~a" (contract-name contract)) given)) 
  
  (define (index-too-large-error who xs idx)
    (arguments-error who "index is too large" "index" idx "in" xs))
  )

(define-syntax (assert stx)
  (syntax-case stx () 
    [(_ expr err-thunk origin) (syntax/loc stx (@assert expr err-thunk origin))]
    [(_ expr err-thunk)        (syntax/loc stx (@assert expr err-thunk #f))]
    [(_ expr)                  (syntax/loc stx (@assert expr #f #f))]))

(define-syntax assert-some 
  (syntax-rules ()
    [(_ expr #:unless size err-thunk origin) 
     (let* ([val expr])
       (unless (= size (length val))
         (assert (apply || (map car val)) err-thunk origin))
       val)]
    [(_ expr #:unless size err-thunk) (assert-some expr #:unless size err-thunk #f)]
    [(_ expr #:unless size)           (assert-some expr #:unless size #f #f)]
    [(_ expr err-thunk origin) (let* ([val expr])
                                 (assert (apply || (map car val)) err-thunk origin)
                                 val)]
    [(_ expr err-thunk) (assert-some expr err-thunk #f)]
    [(_ expr)           (assert-some expr #f #f)]))

(define-syntax assert-|| 
  (syntax-rules ()
    [(_ expr #:unless size err-thunk origin) 
     (let ([val expr])
       (unless (= size (length val))
         (assert (apply || val) err-thunk origin)))]
    [(_ expr #:unless size err-thunk) (assert-|| expr #:unless size err-thunk #f)]
    [(_ expr #:unless size)           (assert-|| expr #:unless size #f #f)]))


(define-syntax assert-bound
  (syntax-rules ()
    [(_ [limit cmp expr] name origin)
     (let ([low limit]
           [high expr])
       (assert (cmp low high)
               (argument-error name (format "~.a ~.a ~.a" low cmp (syntax-e #'expr)) low)))]
    [(_ [limit cmp expr] name)
     (assert-bound [limit cmp expr] name #f)]    
    [(_ [lowLimit cmpLow expr cmpHigh highLimit] name origin)
     (let ([low lowLimit]
           [val expr]
           [high highLimit])
       (assert (cmpLow low val) 
               (argument-error name (format "~.a ~.a ~.a" low cmpLow (syntax-e #'expr)) val))
       (assert (cmpHigh val high)
               (argument-error name (format "~.a ~.a ~.a" (syntax-e #'expr) cmpHigh high) val)))]    
    [(_ [lowLimit cmpLow expr cmpHigh highLimit] name)
     (assert-bound [lowLimit cmpLow expr cmpHigh highLimit] name #f)]))

(define-syntax (assert-arity-includes stx)
  (syntax-case stx ()
    [(_ f val name) (syntax/loc stx 
                      (assert (and (procedure? f) (procedure-arity-includes? f val))
                              (argument-error name
                                              (format "procedure arity includes ~a" val) 
                                              f)))]))

(define (coerce val type [caller-name 'coerce])
  (let-values ([(can-cast? instance) (cast type val)])
    (assert can-cast? (type-error caller-name type val))
    instance))
