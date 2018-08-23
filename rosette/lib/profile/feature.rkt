#lang racket

(require (only-in rosette union? union-contents union expression)
         (only-in rosette/base/core/type get-type typed? type-deconstruct)
         (only-in rosette/base/core/term term?))

(provide (except-out (all-defined-out) flatten-value))



; Stores a procedure that takes as input a list of values
; and outputs a number that characterizes the cost of those values.   
(struct feature (name procedure)
  #:property prop:procedure
  [struct-field-index procedure]
  #:guard (lambda (name proc id)
            (unless (procedure? proc)
              (error 'feature "Expected a procedure?, given ~a" proc))
            (values name proc))
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(feature ~a)" (feature-name self)))])


; A simple feature that returns the sum of the sizes of the input unions.
(define union-size-feature
  (feature
   'union-size
   (lambda (xs)
     (for/sum ([x xs] #:when (union? x))
       (length (union-contents x))))))


; Updates the footprint map to contain the object graph of x.
; The footprint is a set of key-value pairs, where the key is an
; object (a node in the graph), and the value is the number of
; outgoing edges.  Symbolic terms, regardless of size, are treated
; as opaque values with no outgoing edges (just like concrete constants).
(define (measure! footprint x)
  (unless (hash-has-key? footprint x)
    (match x
      [(union gvs)
       (hash-set! footprint x (length gvs))
       (for ([gv gvs]) ; don't count the guards
         (measure! footprint (cdr gv)))]
      [(? list? children)
       (hash-set! footprint x (length children))
       (for ([c children])
         (measure! footprint c))]
      [(cons a b)
       (hash-set! footprint x 2)
       (measure! footprint a)
       (measure! footprint b)]
      [(? vector?)
       (hash-set! footprint x (vector-length x))
       (for ([c x])
         (measure! footprint c))]
      [(box c) 
       (hash-set! footprint x 1)
       (measure! footprint c)]
      [(? typed?)
       (match (type-deconstruct (get-type x) x)
         [(list (== x)) (hash-set! footprint x 0)]
         [children
          (hash-set! footprint x (length children))
          (for ([c children])
            (measure! footprint c))])]
      [_ (hash-set! footprint x 0)])))

       
 
; A simple feature that measures V + E, where V is the number of vertices and
; E is the number of edges that make up the input object graph.
(define heap-size-feature
  (feature
   'heap-size
   (let ([cache (make-hash)])
     (lambda (xs)
       (hash-ref! cache xs
                  (thunk
                   (define footprint (make-hash))
                   (for ([x xs]) (measure! footprint x))
                   (+ (hash-count footprint)
                      (for/sum ([v (in-hash-values footprint)]) v))))))))


; A feature that determines the "signature" of a function call -- the types of
; each of its inputs (union, symbolic, or concrete).
(define signature-feature
  (feature
   'signature
   (lambda (xs)
     (let loop ([xs xs])
       (cond
         [(null? xs) '()]
         [else (let ([x (car xs)])
                 (cons (cond
                         [(union? x) 'union]
                         [(term? x)  'symbolic]
                         [else       'concrete])
                       (loop (cdr xs))))])))))
   

; A parameter that holds a list of features to profile.
(define enabled-features
  (list signature-feature))


; Convert a feature hash into a plain hash with symbols for keys and s-exps for values
(define (flatten-value x)
  (cond
    [(list? x) (map flatten-value x)]
    [(hash? x) (for/hash ([(k v) x]) (values k (flatten-value v)))]
    [(symbol? x) (symbol->string x)]
    [else x]))
(define (features->flat-hash feats)
  (for/hash ([f/v feats])
    (match-define (cons k v) f/v)
    (values (feature-name k) (flatten-value v))))
