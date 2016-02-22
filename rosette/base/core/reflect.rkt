#lang racket

(require (only-in "safe.rkt" coerce) 
         (only-in "forall.rkt" for/all for*/all) 
         "term.rkt" "union.rkt")

(provide type? type-of coerce for/all for*/all
         term? constant? expression? 
         term expression constant
         term-type term=? term-origin term-track-origin
         term->datum clear-terms! term-cache
         union? union union-contents union-guards union-values
         union-filter in-union in-union* in-union-guards in-union-values
         symbolics)

(define (term=? s0 s1)
  (and (term? s0) (term? s1) (equal? s0 s1)))

(define (symbolics vs)
  (match vs
    [(list (? constant?) ...) vs]
    [_ (let ([cache (make-hash)])
         (let loop ([vs vs])
           (hash-ref! 
            cache
            vs
            (lambda ()
              (remove-duplicates 
               (match vs
                 [(union (list (cons guard value) ...))   
                  (append (append-map loop guard) (append-map loop value))]
                 [(expression _ x ...) (append-map loop x)]
                 [(? constant? v) (list v)]
                 [(box v) (loop v)]
                 [(? list?) (append-map loop vs)]
                 [(cons x y) (append (loop x) (loop y))]
                 [(vector v ...) (append-map loop v)]
                 [(and (? typed?) (app get-type t)) 
                    (match (type-deconstruct t vs)
                      [(list (== vs)) '()]
                      [components (append-map loop components)])]
                 [_ '()]))))))]))
