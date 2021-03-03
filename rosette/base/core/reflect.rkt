#lang racket

(require (only-in "forall.rkt" for/all for*/all) 
         "term.rkt" "union.rkt" "result.rkt")

(provide type? solvable? @any/c type-of type-cast for/all for*/all
         term? constant? expression? 
         term expression constant
         term-type term=? term->datum
         terms terms-count terms-ref with-terms clear-terms! gc-terms!
         union? union union-contents union-guards union-values
         union-filter in-union in-union* in-union-guards in-union-values
         (struct-out normal) (struct-out failed) result? result-value result-state
         symbolics concrete? symbolic?)

(define (term=? s0 s1)
  (and (term? s0) (term? s1) (equal? s0 s1)))

(define (symbolics vs)
  (match vs
    [(list (? constant?) ...) (remove-duplicates vs)]
    [(? constant?) (list vs)]
    [_ (let ([terms (mutable-set)]
             [objs  (mutable-set)]
             [result '()])
         (let loop ([datum vs])
           (if (term? datum)
               (let ([id (term-id datum)])
                 (unless (set-member? terms id)
                   (set-add! terms id)
                   (match datum
                     [(expression _ x ...) (for-each loop x)]
                     [(? constant?) (set! result (cons datum result))]))) 
               (unless (set-member? objs datum)
                 (set-add! objs datum)
                 (match datum
                   [(union (list (cons guard value) ...))
                    (for-each loop guard) (for-each loop value)]
                   [(box v) (loop v)]
                   [(? list?) (for-each loop datum)]
                   [(cons x y) (loop x) (loop y)]
                   [(vector v ...) (for-each loop v)]
                   [(and (? typed?) (app get-type t))
                    (match (type-deconstruct t datum)
                      [(list (== datum)) (void)]
                      [components (for-each loop components)])]
                   [_ (void)]))))
         (reverse result))]))

(define (concrete? val)
  (define objs (mutable-set))
  (let all-concrete? ([val val])
    (and (not (term? val))
         (not (union? val))
         (or
          (set-member? objs val)
          (begin
            (set-add! objs val)
            (match val
              [(box v) (all-concrete? v)]
              [(? list?) (for/and ([v val]) (all-concrete? v))]
              [(cons x y) (and (all-concrete? x) (all-concrete? y))]
              [(? vector?) (for/and ([v val]) (all-concrete? v))]
              [(and (? typed?) (app get-type t))
               (match (type-deconstruct t val)
                 [(list (== val)) #t]
                 [components (for/and ([v components]) (all-concrete? v))])]
              [_ #t]))))))

(define (symbolic? val) (not (concrete? val)))
    

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
