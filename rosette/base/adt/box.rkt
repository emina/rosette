#lang racket

(require (for-syntax racket/syntax "../core/lift.rkt") racket/provide 
         "../core/safe.rkt" "generic.rkt"
         (only-in "../core/store.rkt" store!)
         (only-in "../core/type.rkt" define-lifted-type type-cast)
         (only-in "../core/equality.rkt" @eq? @equal?)
         (only-in "../core/bool.rkt" instance-of? && ||)
         (only-in "../core/union.rkt" union)
         (only-in "../core/merge.rkt" merge merge*))

(provide (filtered-out with@ (all-defined-out))
         (rename-out [box @box] [box-immutable @box-immutable]))

(define-lifted-type @box?
  #:base box?
  #:is-a? (instance-of? box? @box?)
  #:methods
  [(define (type-eq? self u v) 
     (or (eq? u v)
         (and (immutable? u) (immutable? v) (@eq? (unbox u) (unbox v)))))
   (define (type-equal? self u v) (@equal? (unbox u) (unbox v)))
   (define (type-cast self v [caller 'type-cast]) (adt-type-cast v #:type box? #:lifted @box? #:caller caller))
   (define (type-compress self force? ps)
     (let*-values ([(immutable mutable) (partition (compose1 immutable? cdr) ps)])
       (append (unsafe/compress box-immutable immutable)
               (if force? (unsafe/compress box mutable) mutable))))
   (define (type-construct self vals) (box (car vals)))
   (define (type-deconstruct self val) (list (unbox val)))])

(define (unsafe/compress box ps)
  (match ps
    [(list) ps]
    [(list _) ps]
    [_  (cons (apply || (map car ps)) 
              (box (apply merge* (for/list ([p ps]) (cons (car p) (unbox (cdr p)))))))]))

(define (@unbox b)
  (match (type-cast @box? b 'unbox)
    [(box v) v]
    [(union vs) (apply merge* (for/list ([gv vs]) (cons (car gv) (unbox (cdr gv)))))]))

(define (box-ref x idx) (unbox x))          ; For the purpose of tracking mutations to the store, 
(define (box-set! x idx v) (set-box! x v))  ; boxes are treated as 1-element vectors that ignore the index argument.

(define (@set-box! b v)
  (match (type-cast @box? b 'set-box!)
    [(? box? x)                        
     (store! x 0 v box-ref box-set!)]
    [(union vs)
     (for ([gv vs])
       (let ([x (cdr gv)])
         (store! x 0 (merge (car gv) v (unbox x)) box-ref box-set!)))]))
     
