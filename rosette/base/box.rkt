#lang racket

(require (for-syntax racket/syntax "lift.rkt") 
         racket/provide 
         "safe.rkt" "lift.rkt" "seq.rkt" "forall.rkt"
         (only-in "effects.rkt" apply!) 
         (only-in "term.rkt" define-type)
         (only-in "equality.rkt" @eq? @equal?)
         (only-in "generic.rkt" make-cast)
         (only-in "any.rkt" @any?)
         (only-in "bool.rkt" instance-of? && ||)
         (only-in "union.rkt" union)
         (only-in "merge.rkt" merge merge*))

(provide (filtered-out with@ (all-defined-out))
         (rename-out [box @box] [box-immutable @box-immutable]))

(define (box/eq? a b)
  (or (eq? a b)
      (and (immutable? a) (immutable? b) (@eq? (unbox a) (unbox b)))))

(define (box/equal? a b)
  (@equal? (unbox a) (unbox b)))

(define (unsafe/compress box ps)
  (match ps
    [(list) ps]
    [(list _) ps]
    [_  (cons (apply || (map car ps)) 
              (box (apply merge* (for/list ([p ps]) (cons (car p) (unbox (cdr p)))))))]))

(define (box/compress force? ps)
  (let*-values ([(immutable mutable) (partition (compose1 immutable? cdr) ps)])
    (append (unsafe/compress box-immutable immutable)
            (if force? (unsafe/compress box mutable) mutable))))

(define-type @box?
  #:pred      (instance-of? box? @box?)
  #:least-common-supertype (lambda (t) (if (eq? t @box?) @box? @any?))
  #:eq?       box/eq?
  #:equal?    box/equal?
  #:cast      (make-cast box? @box?)
  #:compress  box/compress
  #:construct (compose1 box car)
  #:deconstruct (compose1 list unbox))

(define (@unbox b)
  (match (coerce b @box? 'unbox)
    [(box v) v]
    [(union vs) (apply merge* (for/list ([gv vs]) (cons (car gv) (unbox (cdr gv)))))]))

(define (@set-box! b v)
  (match (coerce b @box? 'set-box!)
    [(? box? x)
     (apply! set-box! unbox x v)]
    [(union vs)
     (for ([gv vs])
       (let ([x (cdr gv)])
         (apply! set-box! unbox x (merge (car gv) v (unbox x)))))]))
     
