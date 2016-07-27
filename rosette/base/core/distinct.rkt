#lang racket

(require "term.rkt" "bool.rkt" "real.rkt" "bitvector.rkt" "equality.rkt")

(provide @distinct?)


; Returns true iff all of the given argument values are non-equal to each other
; (i.e., pairwise distinct).
(define distinct?
  (case-lambda
    [()    #t]
    [(x)   #t]
    [(x y) (! (@equal? x y))]
    [xs
     (define t (apply type-of xs))
     (match t
       [(== @boolean?)
        (distinct-primitive-solvable? t 2 => xs)]  
       [(or (== @integer?) (== @real?))
        (distinct-primitive-solvable? t +inf.0 < xs)]
       [(bitvector sz)
        (distinct-primitive-solvable? t (expt 2 sz) (operator-unsafe @bvslt) xs)]
       [_
        (define x (car xs))
        (and-&&
          (apply && 
                 (let loop ([ys (cdr xs)])
                   (cond [(null? ys) null]
                         [else (match (! (@equal? x (car ys)))
                                 [#t (loop (cdr ys))]
                                 [#f (list #f)]
                                 [t (cons t (loop (cdr ys)))])])))
          (apply distinct? (cdr xs)))])]))
        
(define-operator @distinct?
  #:identifier 'distinct?
  #:range T*->boolean?
  #:unsafe distinct?
  #:safe distinct?)        


; Returns true iff all of the given argument values are non-equal to each other
; (i.e., pairwise distinct).  This procedure assumes that each x in xs is a value
; of type t; that t is primitive-solvable?; that c is the cardinality of type t;
; and that t<? is a strict total order over literals of type t.
(define (distinct-primitive-solvable? t c t<? xs)
  (and (<= (length xs) c)
       (let ([xs (for/list ([x xs]) (type-cast t x 'distinct?))])
         (and (= (length xs) (set-count (list->set xs)))
              (let-values ([(terms lits) (partition term? xs)])
                (or (null? terms)
                    (apply expression @distinct? (append (sort lits t<?) (sort terms term<?)))))))))