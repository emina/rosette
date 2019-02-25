#lang racket

(require "term.rkt" "union.rkt" "bool.rkt")

(provide @eq?     ; (-> any/c any/c @boolean?)
         @equal?) ; (-> any/c any/c @boolean?)

; We must use identity-based hashing and comparison of user-provided values,
; because user-defined structs can override equal/hash and cause unexpected
; errors when the overriden equal? is repeatedly called by a hash map. We also
; have to use (below) identity-based comparisons for shortcircuiting for the
; same reason---equal? might be overriden by a user-defined struct.
(struct key (x y)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (and (eq? (key-x a) (key-x b))
          (eq? (key-y a) (key-y b))))
   (define (hash-proc a hash-recur)
     (hash-recur (cons (eq-hash-code (key-x a)) (eq-hash-code (key-y a)))))
   (define (hash2-proc a hash2-recur)
     (hash2-recur (cons (eq-hash-code (key-y a)) (eq-hash-code (key-x a)))))])

(define-syntax-rule (define-equality-predicate @=? type=? @cache @make-hash)
  (define (@=? x y)
    (let* ([cache (@cache)]
           [toplevel? (false? cache)]
           [k (key x y)])
      (when toplevel?
        (set! cache (@make-hash))
        (@cache cache))
      (if (hash-has-key? cache k)
          (hash-ref cache k)
          (begin
            (hash-set! cache k #t)
            (let ([result
                   (cond [(eq? x y) #t] ; We must use identity-based comparisons for short-circuiting.
                         [(union? x) (if (union? y) 
                                         (union=union? x y @=?) 
                                         (union=value? x y @=?))]
                         [(union? y) (union=value? y x @=?)]
                         [else (type=? (type-of x y) x y)])])
              (if toplevel?
                  (@cache #f)
                  (hash-set! cache k result))
              result))))))
                   
                

(define equal-cache (make-parameter #f))
(define eq-cache (make-parameter #f))

(define-equality-predicate @equal? type-equal? equal-cache make-hash)
(define-equality-predicate @eq? type-eq? eq-cache make-hash)

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

