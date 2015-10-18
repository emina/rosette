#lang racket

(require (for-syntax racket/syntax "../core/lift.rkt") 
         racket/provide 
         "../core/safe.rkt" 
         (only-in "../core/effects.rkt" apply!) 
         (only-in "../core/term.rkt" lift-type @any/c)
         (only-in "../core/equality.rkt" @eq? @equal?)
         (only-in "../core/generic.rkt" make-cast)
         (only-in "../core/bool.rkt" instance-of? && ||)
         (only-in "../core/union.rkt" union)
         (only-in "../core/merge.rkt" merge merge*))

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

(define @box?
  (lift-type
   box?
   #:is-a?     (instance-of? box? @box?)
   #:least-common-supertype (lambda (t) (if (eq? t @box?) @box? @any/c))
   #:eq?       box/eq?
   #:equal?    box/equal?
   #:cast      (make-cast box? @box?)
   #:compress  box/compress
   #:construct (compose1 box car)
   #:deconstruct (compose1 list unbox)))

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
     
