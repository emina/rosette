#lang racket

(provide (rename-out [syntax->location location])
         location? location-source location-start location-end
         location-contains? read-module
         path-string->string
         syntax->readable-location
         rebuild
         make-transform
         keep-lambda-properties
         rebuild
         make-transform
         define-disarm
         make-add-annotate-property)

(require syntax/parse/define
         syntax/parse
         (for-syntax racket/base))

(struct location (source start end) #:transparent)

(define (syntax->location stx) 
  (let ([pos (syntax-position stx)]
        [span (syntax-span stx)])
    (and pos span (location (syntax-source stx) pos (+ pos span)))))

(define (location-contains? outer inner)
  (and (equal? (location-source outer) (location-source inner))
       (<= (location-start outer) (location-start inner))
       (>= (location-end outer) (location-end inner))))

(define (read-module path)
  (parameterize ([read-accept-reader #t]
                 [port-count-lines-enabled #t]) 
    (read-syntax path (open-input-file path))))

(define (syntax->readable-location stx)
  (list (path-string->string (syntax-source stx))
        (syntax-line stx)
        (syntax-column stx)))

;; Convert a path to a string
(define (path-string->string path)
  (if (path? path) (path->string path) path))

(define (keep-lambda-properties orig new)
  (let* ([new (let ([p (syntax-property orig 'method-arity-error)])
                (if p
                    (syntax-property new 'method-arity-error p)
                    new))]
         [new (let ([p (syntax-property orig 'inferred-name)])
                (if p
                    (syntax-property new 'inferred-name p)
                    new))])
    new))

(define (rebuild expr replacements)
  (let loop ([expr expr] [same-k (lambda () expr)] [diff-k (lambda (x) x)])
    (let ([a (assq expr replacements)])
      (cond
        [a (diff-k (cdr a))]
        [(pair? expr)
         (loop (car expr)
               (lambda ()
                 (loop (cdr expr) same-k
                       (lambda (y) (diff-k (cons (car expr) y)))))
               (lambda (x)
                 (loop (cdr expr)
                       (lambda () (diff-k (cons x (cdr expr))))
                       (lambda (y) (diff-k (cons x y))))))]
        [(vector? expr)
         (loop (vector->list expr) same-k
               (lambda (x) (diff-k (list->vector x))))]
        [(box? expr)
         (loop (unbox expr) same-k (lambda (x) (diff-k (box x))))]
        [(syntax? expr)
         (if (identifier? expr)
             (same-k)
             (loop (syntax-e expr) same-k
                   (lambda (x) (diff-k (datum->syntax expr x expr expr)))))]
        [else (same-k)]))))


(define ((make-transform expr disarmed-expr annotate phase rearm)
         xs
         #:expr [e disarmed-expr]
         #:annotate [ann annotate]
         #:phase [ph phase]
         #:rearm? [rearm? #t])
  (define rebuilt (rebuild e (map (λ (x) (cons x (ann x ph))) xs)))
  (if rearm? (rearm expr rebuilt) rebuilt))

(define-simple-macro (define-disarm disarm:id inspector)
  (define-syntax disarm
    (pattern-expander
     (syntax-parser
       [(_ pattern) #'(~and x (~parse pattern (syntax-disarm #'x inspector)))]))))

(define ((make-add-annotate-property disarm annotate-key) s)
  (let add-annotate-property ([s s])
    (cond
      [(syntax? s)
       (define new-s (syntax-rearm
                      (let ([s (disarm s)])
                        (datum->syntax s
                                       (add-annotate-property (syntax-e s))
                                       s
                                       s))
                      s))
       (syntax-property new-s annotate-key #t #t)]
      [(pair? s)
       (cons (add-annotate-property (car s))
             (add-annotate-property (cdr s)))]
      [(vector? s)
       (for/vector #:length (vector-length s) ([e (in-vector s)])
         (add-annotate-property e))]
      [(box? s) (box (add-annotate-property (unbox s)))]
      [(prefab-struct-key s)
       => (lambda (k)
            (apply make-prefab-struct
                   k
                   (add-annotate-property (cdr (vector->list (struct->vector s))))))]
      [(and (hash? s) (immutable? s))
       (cond
         [(hash-eq? s)
          (for/hasheq ([(k v) (in-hash s)])
            (values k (add-annotate-property v)))]
         [(hash-eqv? s)
          (for/hasheqv ([(k v) (in-hash s)])
            (values k (add-annotate-property v)))]
         [else
          (for/hash ([(k v) (in-hash s)])
            (values k (add-annotate-property v)))])]
      [else s])))
