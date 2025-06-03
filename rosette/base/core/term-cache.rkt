#lang racket/base

(provide prop:term-cachable
         term-cache-weak?
         make-term-cache
         make-weak-term-cache
         term-cache-ref
         term-cache-set!
         term-cache-count
         term-cache-copy
         term-cache-copy-clear
         term-cache->hash)

(require racket/match)

(struct term-cache (nullary unary binary ternary nary))

(define-values (prop:term-cachable term-cachable? term-cachable-ref)
  (make-struct-type-property 'term-cachable))

(define (term-cache-weak? x)
  (and (hash? x) (hash-ephemeron? x)))

(define (make-term-cache [assocs '()])
  (define out
    (term-cache (make-hasheq)
                (make-hasheq)
                (make-hasheq)
                (make-hasheq)
                (make-hash)))
  (for ([pair (in-list assocs)])
    (term-cache-set! out (car pair) (cdr pair)))
  out)

(define make-weak-term-cache make-ephemeron-hash)

(define (term-like? x)
  (or (term-cachable? x) (syntax? x) (fixnum? x) (boolean? x) (procedure? x)))

(define (term-cache-ref h k default)
  (define (proc) (if (procedure? default) (default) default))
  (match h
    [(term-cache nullary unary binary ternary nary)
     (match k
       [_
        #:when (term-like? k)
        (hash-ref nullary k proc)]
       [(list op a)
        #:when (and (term-like? op) (term-like? a))
        (match (hash-ref unary op #f)
          [#f (proc)]
          [h (hash-ref h a proc)])]
       [(list op a b)
        #:when (and (term-like? op) (term-like? a) (term-like? b))
        (match (hash-ref binary op #f)
          [#f (proc)]
          [h
           (match (hash-ref h a #f)
             [#f (proc)]
             [h (hash-ref h b proc)])])]
       [(list op a b c)
        #:when (and (term-like? op) (term-like? a) (term-like? b) (term-like? c))
        (match (hash-ref ternary op #f)
          [#f (proc)]
          [h
           (match (hash-ref h a #f)
             [#f (proc)]
             [h
              (match (hash-ref h b #f)
                [#f (proc)]
                [h (hash-ref h c proc)])])])]
       [_ (hash-ref nary k proc)])]
    [_ (hash-ref h k proc)]))

(define (term-cache-set! h k v)
  (match h
    [(term-cache nullary unary binary ternary nary)
     (match k
       [_
        #:when (term-like? k)
        (hash-set! nullary k v)]
       [(list op a)
        #:when (and (term-like? op) (term-like? a))
        (hash-set! (hash-ref! unary op make-hasheq) a v)]
       [(list op a b)
        #:when (and (term-like? op) (term-like? a) (term-like? b))
        (hash-set! (hash-ref! (hash-ref! binary op make-hasheq) a make-hasheq) b v)]
       [(list op a b c)
        #:when (and (term-like? op) (term-like? a) (term-like? b) (term-like? c))
        (hash-set! (hash-ref! (hash-ref! (hash-ref! ternary op make-hasheq) a make-hasheq) b make-hasheq) c v)]
       [_ (hash-set! nary k v)])]
    [_ (hash-set! h k v)]))

(define (term-cache-count h)
  (match h
    [(term-cache nullary unary binary ternary nary)
     (+ (hash-count nullary)
        (for/sum ([h (in-hash-values unary)])
          (hash-count h))
        (for/sum ([h (in-hash-values binary)])
          (for/sum ([h (in-hash-values h)])
            (hash-count h)))
        (for/sum ([h (in-hash-values ternary)])
          (for/sum ([h (in-hash-values h)])
            (for/sum ([h (in-hash-values h)])
              (hash-count h))))
        (hash-count nary))]
    [_ (hash-count h)]))

(define (term-cache-copy h)
  (match h
    [(term-cache nullary unary binary ternary nary)
     (term-cache (hash-copy nullary)
                 (hash-copy unary)
                 (hash-copy binary)
                 (hash-copy ternary)
                 (hash-copy nary))]
    [_ (hash-copy h)]))

(define (term-cache-copy-clear h)
  ;; NOTE: we don't use hash-copy-clear because hash-copy-clear for Racket 8.1
  ;; is buggy. See https://github.com/racket/racket/pull/4094
  (cond
    [(term-cache-weak? h) (make-weak-term-cache)]
    [else (make-term-cache)]))

(define (term-cache->hash h term-val)
  ;; NOTE: it is important that we use (term-val v) instead of reconstruct
  ;; a hash key ourselves. Otherwise, the GC can potentially consider these
  ;; freshly reconstructed terms unreachable, as they are not eq? to values
  ;; that are currently reachable. In particular, after we gc-terms!,
  ;; which switches the representation to ephemeron hash,
  ;; the reconstructed terms would truly be unreachable, incorrectly dropping
  ;; terms from the cache.
  (match h
    [(term-cache nullary unary binary ternary nary)
     (define h* (hash-copy nary))
     (for ([v (in-hash-values nullary)])
       (hash-set! h* (term-val v) v))
     (for ([h (in-hash-values unary)])
       (for ([v (in-hash-values h)])
         (hash-set! h* (term-val v) v)))
     (for ([h (in-hash-values binary)])
       (for ([h (in-hash-values h)])
         (for ([v (in-hash-values h)])
           (hash-set! h* (term-val v) v))))
     (for ([h (in-hash-values ternary)])
       (for ([h (in-hash-values h)])
         (for ([h (in-hash-values h)])
           (for ([v (in-hash-values h)])
             (hash-set! h* (term-val v) v)))))
     h*]
    [_ h]))
