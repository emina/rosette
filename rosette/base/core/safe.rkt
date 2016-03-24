#lang racket

(require (only-in "type.rkt" type-cast)
         "bool.rkt"
         racket/performance-hint)

(provide argument-error arguments-error type-error contract-error index-too-large-error
         assert assert-some assert-|| assert-bound assert-arity-includes)

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
    [(_ expr err-thunk)        (syntax/loc stx (@assert expr err-thunk))]
    [(_ expr)                  (syntax/loc stx (@assert expr #f))]))

(define-syntax assert-some 
  (syntax-rules ()
    [(_ expr #:unless size err-thunk) 
     (let* ([val expr])
       (unless (= size (length val))
         (assert (apply || (map car val)) err-thunk))
       val)]
    [(_ expr #:unless size)
     (assert-some expr #:unless size #f)]
    [(_ expr err-thunk)
     (let* ([val expr])
       (assert (apply || (map car val)) err-thunk)
       val)]
    [(_ expr)
     (assert-some expr #f)]))

(define-syntax assert-|| 
  (syntax-rules ()
    [(_ expr #:unless size err-thunk) 
     (let ([val expr])
       (unless (= size (length val))
         (assert (apply || val) err-thunk)))]
    [(_ expr #:unless size)           (assert-|| expr #:unless size #f)]))


(define-syntax assert-bound
  (syntax-rules ()
    [(_ [limit cmp expr] name)
     (let ([low limit]
           [high expr])
       (assert (cmp low high)
               (argument-error name (format "~.a ~.a ~.a" low cmp (syntax-e #'expr)) low)))]   
    [(_ [lowLimit cmpLow expr cmpHigh highLimit] name)
     (let ([low lowLimit]
           [val expr]
           [high highLimit])
       (assert (cmpLow low val) 
               (argument-error name (format "~.a ~.a ~.a" low cmpLow (syntax-e #'expr)) val))
       (assert (cmpHigh val high)
               (argument-error name (format "~.a ~.a ~.a" (syntax-e #'expr) cmpHigh high) val)))]))

(define-syntax (assert-arity-includes stx)
  (syntax-case stx ()
    [(_ f val name) (syntax/loc stx 
                      (assert (and (procedure? f) (procedure-arity-includes? f val))
                              (argument-error name
                                              (format "procedure arity includes ~a" val) 
                                              f)))]))
