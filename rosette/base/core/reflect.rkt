#lang racket

(require (only-in "forall.rkt" for/all for*/all) 
         "term.rkt" "union.rkt")

(provide type? solvable? @any/c type-of type-cast for/all for*/all
         term? constant? expression? 
         term expression constant
         term-type term=? 
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
           (if (hash-has-key? cache vs)
               (hash-ref cache vs)
               (begin
                (hash-set! cache vs '())
                (let ([result
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
                          [_ '()]))])
                  (hash-set! cache vs result)
                  result)))))]))

(define (term->datum val)
  (let convert ([val val] [cache (make-hash)])
    (if (hash-has-key? cache val) 
        (hash-ref cache val)
        (let ([datum
               (match val
                 [(? constant?) (string->symbol (format "~a" val))]
                 [(expression op child ...) `(,(string->symbol (~s op))
                                              ,@(for/list ([e child]) (convert e cache)))]
                 [_  val])])
          (hash-set! cache val datum)
          datum))))
