#lang racket

(require "term.rkt" "union.rkt" "bool.rkt")

(provide @eq?     ; (-> any/c any/c @boolean?)
         @equal?) ; (-> any/c any/c @boolean?)


(define-syntax-rule (define-equality-predicate @=? =? type=? @cache @make-hash)
  (define (@=? x y)
    (let* ([cache (@cache)]
           [toplevel? (false? cache)]
           [key (cons x y)])
      (when toplevel?
        (set! cache (@make-hash))
        (@cache cache))
      (if (hash-has-key? cache key)
          (hash-ref cache key)
          (begin
            (hash-set! cache key #t)
            (let ([result
                   (cond [(=? x y) #t]
                         [(union? x) (if (union? y) 
                                         (union=union? x y @=?) 
                                         (union=value? x y @=?))]
                         [(union? y) (union=value? y x @=?)]
                         [else (type=? (type-of x y) x y)])])
              (if toplevel?
                  (@cache #f)
                  (hash-set! cache key result))
              result))))))
                   
                

(define equal-cache (make-parameter #f))
(define eq-cache (make-parameter #f))
(define-equality-predicate @equal? equal? type-equal? equal-cache make-hash)
(define-equality-predicate @eq? eq? type-eq? eq-cache make-hasheq)

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

