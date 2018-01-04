#lang racket

(provide odict? (rename-out [make-odict odict]))

(struct odict (tbl [ord #:mutable] eq)
  #:methods gen:dict
    [(define (dict-ref dict key [default (lambda () (error "key not found" key))])
       (hash-ref (odict-tbl dict) key default))

     (define (dict-set! dict key val)
       (match-define (odict tbl ord _) dict)
       (unless (hash-has-key? tbl key)
         (set-odict-ord! dict (cons key ord)))
       (hash-set! tbl key val))

     (define (dict-remove! dict key)
       (match-define (odict tbl ord eq) dict)
       (when (hash-has-key? tbl key)
         (hash-remove! tbl key)
         (set-odict-ord! dict (remove key ord eq))))

     (define (dict-iterate-first dict)
       (match-define (odict _ ord _) dict)
       (and (not (null? ord)) ord))

     (define (dict-iterate-next dict pos)
       (and (> (length pos) 1) (cdr pos)))

     (define (dict-iterate-key dict pos)
       (car pos))

     (define (dict-iterate-value dict pos)
       (hash-ref (odict-tbl dict) (car pos)))

     (define (dict-count dict)
       (hash-count (odict-tbl dict)))

     (define (dict-has-key dict key)
       (hash-has-key? (odict-tbl dict) key))
     ])

(define (make-odict [assocs null] [is-equal? equal?])
  (define make-table
    (match is-equal?
      [(== equal?) make-hash]
      [(== eq?) make-hasheq]
      [(== eqv?) make-hasheqv]
      [_ (error 'odict "expected equal?, eq?, or eqv? equivalence predicate, given ~a" is-equal?)]))
  (odict (make-table assocs) (map car assocs) is-equal?))

   