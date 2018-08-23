#lang racket

(require (only-in rnrs/base-6 assert)
         (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
         "term.rkt" "union.rkt" "bool.rkt" "reporter.rkt")

(provide merge merge* unsafe-merge* merge-same)

(define (merge b x y)
  (match* (b x y)
    [(#t _ _) x]
    [(#f _ _) y]
    [(_ _ (== x eq?)) x]
    [(_ _ _) (merge* (cons b x) (cons (! b) y))]))

; Returns a value that joins the provided values 
; according to their guards.  In particular, this 
; procedure assumes that the guards are constrained
; in such a way that *at most one* of them is true 
; in any model of the overall problem.
(define (merge* . ps)
  (do-merge* #f ps))  

; Returns a value that joins the provided values 
; according to their guards.  In particular, this 
; procedure assumes that the guards are constrained
; in such a way that *at most one* of them is true 
; in any model of the overall problem.  Unlike, merge*
; unsafe-merge* forces merging of mutable values whenever
; possible.
(define (unsafe-merge* . ps)
  (do-merge* #t ps))

(define-syntax-rule (do-merge* force? ps)
  (let ([simp (simplify ps)])
    ((current-reporter) 'merge (length simp))
    (match (compress force? simp)
      [(list (cons g v)) (assert (not (false? g))) v]
      [(list _ (... ...) (cons #t v) _ (... ...)) v]
      [vs (apply union vs)])))

(define (guard-&& a b)
  (match b  
    [(expression (== @&&) c ...) (apply && a c)]
    [_ (&& a b)])) 

(define (guard g vs)
  (filter-map (lambda (v) 
                (let ([gv (guard-&& g (car v))])
                  (and gv (cons gv (cdr v)))))
              vs))

(define (simplify ps)
  (let loop ([ps ps] [out '()])
    (match ps
      [(list) out]
      [(list (and (cons #t v) p) _ ...) 
       (list p)]
      [(list (cons #f _) rest ...) 
       (loop rest out)]
      [(list (cons g (union (and (not (? null?)) vs))) rest ...) 
       (loop rest (append (guard g vs) out))]
      [(list p rest ...) 
       (loop rest (cons p out))])))

(define (group ps)
  (let ([types (remove-duplicates (for/list ([p ps]) (type-of (cdr p))))])
    (for*/list ([t types] [p ps] #:when (equal? t (type-of (cdr p)))) p)))

(define (compress force? ps)
  (match ps
    [(list _) ps] 
    [(list (cons g (app type-of t)) (cons h (app type-of t))) 
     (type-compress t force? (merge-same ps))]
    [(list _ _) ps]
    [_ (let loop ([ps (group ps)] [type #f] [acc '()])
         ;(printf "compress ~a ~a ~a\n" ps type acc)
         (match ps
           [(list) 
            (append-map (lambda (group) 
                          (type-compress (type-of (cdar group)) 
                                         force? 
                                         (merge-same group))) 
                        acc)]
           [(list (and (cons _ (app type-of (== type))) p) rest ...)
            (loop rest type (cons (cons p (car acc)) (cdr acc)))]
           [(list p rest ...)
            (loop rest (type-of (cdr p)) (cons (list p) acc))]))]))

(define (merge-same ps)
  ;(printf "merge ~a\n" ps)
  (match ps
    [(or (list) (list _)) ps]
    [(list (cons g v) (cons h u)) (if (eq? v u) (list (cons (|| g h) v)) ps)]
    [_ (let loop ([ps ps] [out '()])
         (if (null? ps)
             out
             (match-let*-values 
              ([((cons g v)) (car ps)]
               [((list (cons h _) ...) rest) (partition (compose (curry eq? v) cdr) (cdr ps))]
               [(g) (apply || g h)])
              (if (equal? g #t)
                  (list (cons g v))
                  (loop rest (cons (cons g v) out))))))]))
