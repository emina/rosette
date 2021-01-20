#lang racket

(require (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
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
      [(list (cons g v)) v]
      [(list _ (... ...) (cons #t v) _ (... ...)) v]
      [vs (apply union vs)])))

(define (guard g gvs)
  (for*/list ([gv gvs]
              [gg (in-value (&& g (car gv)))]
              #:when gg)
    (cons gg (cdr gv))))

(define (simplify ps)
  (match ps
    [(list _ ... (and (cons #t _) p) _ ...)
     (list p)]
    [_ (for/fold ([out '()]) ([p ps])
         (match p
           [(cons #f _) out]
           [(cons g (union (and (not (? null?)) gvs)))
            (append (guard g gvs) out)] 
           [_ (cons p out)]))]))

(define (type-of-value gv) (type-of (cdr gv)))

(define (compress force? ps)
  (match ps
    [(list _) ps] 
    [(list (cons _ (app type-of t)) (cons _ (app type-of t))) 
     (type-compress t force? (merge-same ps))]
    [(list _ _) ps]
    [_ (append-map
        (lambda (group)
          (type-compress
           (type-of (cdar group)) 
           force? 
           (merge-same group)))
        (group-by type-of-value ps))]))

(define (merge-same ps)
  (match ps
    [(or (list) (list _)) ps]
    [(list (cons g v) (cons h u))
     (if (eq? v u) (list (cons (|| g h) v)) ps)]
    [_ (let loop ([ps (group-by cdr ps eq?)] [out '()])
         (match ps
           [(list) out]
           [(list (list gv) rest ...)
            (loop rest (cons gv out))]
           [(list group rest ...)
            (let ([g (apply || (map car group))]
                  [v (cdar group)])
              (if (eq? g #t)
                  (list (cons g v))
                  (loop rest (cons (cons g v) out))))]))]))
