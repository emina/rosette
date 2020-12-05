#lang racket

(require "bool.rkt" "exn.rkt")

(provide argument-error arguments-error type-error contract-error index-too-large-error
         assert assert-some assert-|| assert-bound assert-arity-includes)

  
(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ expr)      (syntax/loc stx ($assert expr #f))]
    [(_ expr msg)  (syntax/loc stx ($assert expr msg))]))

(define-syntax assert-some 
  (syntax-rules ()
    [(_ expr #:unless size msg) 
     (let* ([val expr])
       (unless (= size (length val))
         (assert (apply || (map car val)) msg))
       val)]
    [(_ expr #:unless size)
     (assert-some expr #:unless size #f)]
    [(_ expr msg)
     (let* ([val expr])
       (assert (apply || (map car val)) msg)
       val)]
    [(_ expr)
     (assert-some expr #f)]))

(define-syntax assert-|| 
  (syntax-rules ()
    [(_ expr #:unless size msg) 
     (let ([val expr])
       (unless (= size (length val))
         (assert (apply || val) msg)))]
    [(_ expr #:unless size) (assert-|| expr #:unless size #f)]))


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
