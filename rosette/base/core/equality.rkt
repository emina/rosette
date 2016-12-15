#lang racket

(require "term.rkt" "union.rkt" "bool.rkt")

(provide @eq?     ; (-> any/c any/c @boolean?)
         @equal?) ; (-> any/c any/c @boolean?)


(define ustack (make-parameter '()))

(define-syntax-rule (define-equality-predicate @=? =? type=?)
  (define (@=? x y)
    (if (=? x y)
        #t
        (let ([args (cons x y)])
          (if (member args (ustack))
              #t
              (parameterize ([ustack (cons args (ustack))])
                (cond 
                  [(union? x) (if (union? y) 
                                  (union=union? x y @=?) 
                                  (union=value? x y @=?))]
                  [(union? y) (union=value? y x @=?)]
                  [else (type=? (type-of x y) x y)])))))))

(define-equality-predicate @equal? equal? type-equal?)
(define-equality-predicate @eq? eq? type-eq?)

; (-> union? union? (-> any/c any/c @boolean?) @boolean?)
(define (union=union? x y =?)
  (match* (x y)
    [((union vs t) (union ws s))
     (and (or (subtype? t s) (subtype? s t))
          (apply || (for*/list ([v vs] [w ws]) 
                      (and-&&
                       (=? (cdr v) (cdr w))
                       (car v)
                       (car w)))))]))

; (-> union? (not/c union?) (-> any/c any/c @boolean?) @boolean?)
(define (union=value? x y =?)
  (match* (x y)
    [((union vs t) (app type-of s))
     (and (or (subtype? t s) (subtype? s t))
          (apply || (for/list ([v vs]) (and-&& (=? y (cdr v)) (car v)))))]))

