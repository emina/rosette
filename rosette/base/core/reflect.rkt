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
    [(list (? constant?) ...) (remove-duplicates vs)]
    [_ (let ([cache (mutable-set)]
             [result '()])
         (let loop ([vs vs])
           (unless (set-member? cache vs)
             (set-add! cache vs)
             (match vs
               [(union (list (cons guard value) ...))
                (for-each loop guard) (for-each loop value)]
               [(expression _ x ...) (for-each loop x)]
               [(? constant? v) (set! result (cons v result))]
               [(box v) (loop v)]
               [(? list?) (for-each loop vs)]
               [(cons x y) (loop x) (loop y)]
               [(vector v ...) (for-each loop v)]
               [(and (? typed?) (app get-type t))
                (match (type-deconstruct t vs)
                  [(list (== vs)) (void)]
                  [components (for-each loop components)])]
               [_ (void)])))
         (reverse result))]))

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
