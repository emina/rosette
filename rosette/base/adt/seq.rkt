#lang racket

(require (for-syntax racket/syntax "../core/lift.rkt") 
         racket/splicing racket/stxparam
         (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
         "../core/safe.rkt" "../core/lift.rkt"
         (only-in "../core/type.rkt" type-cast)
         (only-in "../core/bool.rkt" && or-|| ||)
         (only-in "../core/real.rkt" @integer? @= @< @<=)
         (only-in "../core/union.rkt" union union?)
         (only-in "../core/merge.rkt" merge merge* unsafe-merge*)
         (only-in "../core/forall.rkt" guard-apply))

(provide seq-compress
         lift/apply/higher-order higher-order/for 
         define/lift/ref define/lift/append define/lift/split)

(define-syntax-rule 
  (higher-order/for (vs) #:lift (applicator proc arg ...) #:enforce rosette-contract? #:name name)
  (guard-apply (lambda (v) (applicator proc arg ... v)) vs))

(define-syntax lift/apply/higher-order
  (syntax-rules (: ->)
    [(_ applicator proc arg ... seq : name : racket-contract? -> rosette-contract?)
     (match (type-cast rosette-contract? seq name)
       [(? racket-contract? vs) (applicator proc arg ... vs)]
       [(union vs) (higher-order/for (vs) #:lift (applicator proc arg ...) #:enforce rosette-contract? #:name name)])]
    [(_ applicator proc arg ... seq : racket-contract? -> rosette-contract?)
     (lift/apply/higher-order applicator proc arg ... seq : (quote applicator) : racket-contract? -> rosette-contract?)]))

(define (@ref vs idx)
  ;(printf "@ref ~a ~a\n" (if (vector? vs) 'vector 'list) idx)
  (apply merge* (for/list ([(v i) (in-indexed vs)])
                (cons (@= i idx) v))))

(define-syntax (define/lift/ref stx)
  (syntax-case stx (: ->)
    [(_ proc : (racket-contract? racket-length) -> rosette-contract?)
     #`(define (#,(lift-id #'proc) xs idx)
         (if (and (racket-contract? xs) (number? idx))
             (proc xs idx)
             (match* ((type-cast rosette-contract? xs (quote proc))
                      (type-cast @integer? idx (quote proc)))
               [((? racket-contract? vs) (? number? idx)) 
                (proc vs idx)]
               [((? racket-contract? vs) idx)
                (assert-bound [0 @<= idx @< (racket-length vs)] (quote proc))
                (@ref vs idx)]
               [((union vs) (? number? idx))
                (assert-bound [0 <= idx] (quote proc))
                (apply merge* (assert-some 
                             (for/list ([v vs] #:when (< idx (racket-length (cdr v))))
                               (cons (car v) (proc (cdr v) idx)))
                             #:unless (length vs)
                             (index-too-large-error (quote proc) xs idx)))]
               [((union vs) idx)
                (assert-bound [0 @<= idx @< (merge** vs racket-length)] (quote proc))
                (merge** vs (@ref _ idx))])))]))

(define-syntax (define/lift/append stx)
  (syntax-case stx (:) 
    [(_ proc : (racket-contract? racket-constructor) -> rosette-contract?)
     #`(splicing-local
           [(define (unsafe/append xs ys)
              (match* (xs ys)
                [((? racket-contract?) (? racket-contract?)) (proc xs ys)]
                [((racket-constructor) _) ys]
                [(_ (racket-constructor)) xs]
                [((? racket-contract?) (union vs)) (unsafe-merge** vs (proc xs _))]
                [((union vs) (? racket-contract?)) (unsafe-merge** vs (proc _ ys))] 
                [((union vs) (union ws))
                 (apply unsafe-merge* 
                        (assert-some 
                         (for*/list ([v vs] [w ws] [g (in-value (&& (car v) (car w)))] #:when g)
                           (cons g (proc (cdr v) (cdr w))))
                         #:unless (* (length vs) (length ws))
                         (arguments-error (quote proc) (format "expected ~a ~a" rosette-contract? rosette-contract?)
                                          "first argument" vs "second argument" ws)))]))] 
         (define #,(lift-id #'proc)
           (case-lambda 
             [()      (racket-constructor)]
             [(xs)    (type-cast rosette-contract? xs (quote proc))]
             [(xs ys) (unsafe/append (type-cast rosette-contract? xs (quote proc)) 
                                     (type-cast rosette-contract? ys (quote proc)))]                                               
             [xss     (for/fold ([out (racket-constructor)])
                        ([xs (for/list ([ys xss]) (type-cast rosette-contract? ys (quote proc)))])
                        (unsafe/append out xs))])))]))

(define-syntax (define/lift/split stx)
  (syntax-case stx ()
    [(_ proc left right)
     #`(define (#,(lift-id #'proc) xs idx)
         (if (and (not (union? xs)) (number? idx))
             (proc xs idx)
             (match* (xs (type-cast @integer? idx (quote proc)))
               [((not (? union?)) (? number? idx)) (proc xs idx)]
               [(_ idx) (values (left xs idx) (right xs idx))])))]))

(define-syntax-rule (all-same? proc a b ...) 
  (let ([v (proc a)])
    (and (eq? v (proc b)) ...))) 

(define-syntax-parameter for/seq (syntax-rules ()))

(define-syntax (do-compress stx)
  (syntax-case stx ()
    [(_ (g vs) ...)
     (with-syntax ([(v ...) (generate-temporaries #'(vs ...))])
       #'(cons (or-|| g ...) (for/seq ([v vs] ...) (merge* (cons g v) ...))))]))

; Compresses a list of guarded sequences, merging all sequences of the 
; same length into a single sequence with guarded entries (forced merge).
(define-syntax seq-compress
  (syntax-rules (:) 
    [(seq-compress ps seq-length seq-map : for/sequence)
     (syntax-parameterize 
      ([for/seq (syntax-rules () for/sequence)])
      (match ps
        [(list _) ps]
        [(list (cons g xs) (cons h ys))       
         (if (all-same? seq-length xs ys) (list (do-compress [g xs] [h ys])) ps)]
        [_ (let loop ([ps (sort ps < #:key (compose seq-length cdr))] [len #f] [acc '()])
             (match ps
               [(list) 
                (for/list ([group acc])
                  (match group
                    [(list elt) elt]
                    [(list (cons g xs) (cons h ys)) (do-compress [g xs] [h ys])]
                    [(list (cons g xs) (cons h ys) (cons f zs)) (do-compress [g xs] [h ys] [f zs])]
                    [(list (cons g xs) (cons h ys) (cons f zs) (cons k vs)) (do-compress [g xs] [h ys] [f zs] [k vs])]
                    [_  (cons (apply || (map car group))
                              (apply seq-map merge* (for/list ([g group]) 
                                                    (seq-map (curry cons (car g)) (cdr g)))))]))]
               [(list (and (cons _ (app seq-length (== len))) p) rest (... ...)) 
                (loop rest len (cons (cons p (car acc)) (cdr acc)))]
               [(list p rest (... ...))
                (loop rest (seq-length (cdr p)) (cons (list p) acc))]))]))]))


