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
         define-disarm)

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
