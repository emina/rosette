#lang racket

(require racket/generic
         (for-syntax syntax/transformer)
         "term.rkt" "bool.rkt" "safe.rkt" "union.rkt"  "equality.rkt"  "merge.rkt"
         (only-in "procedure.rkt" @procedure?))

(provide (rename-out [fv-stx fv]) @fv? fv? fv-type
         ~> function function? function-domain function-range)

#|-----------------------------------------------------------------------------------|#
; A function type is a solvable applicable type.  That is, it implements the solvable?
; interface and its type-applicable? method returns true.  The domain of a function type
; is a non-empty list of primitive-solvable? types, and its range is a primitive-solvable?
; type.
;
; The only values that have function types are instances of the fv struct.
; An fv value is a procedure and can be directly applied to values
; (symbolic, concrete, or a mix of the two).
#|-----------------------------------------------------------------------------------|#

; Represents a function type.
(struct function (domain range)
  #:transparent
  #:guard (lambda (dom ran name)
            (when (null? dom)
              (error name "expected a non-empty list of domain types"))
            (for ([t dom] #:unless (primitive-solvable? t))
              (raise-arguments-error name "expected a list of primitive solvable types" "domain" dom))
            (unless (primitive-solvable? ran)
              (raise-arguments-error name "expected a primitive solvable type" "range" ran))
            (values dom ran))
  #:property prop:procedure ; Recognizes functions of this type.
  (lambda (self v)
    (match v
      [(? typed? (app get-type (== self))) #t]
      [(union _ (or (== @procedure?) (== @any/c)))
       (apply || (for/list ([g (in-union-guards v self)]) g))]
      [_ #f]))
  #:methods gen:type
  [(define (least-common-supertype self other)
     (cond [(equal? self other) self]
           [(type-applicable? other) @procedure?]
           [else @any/c]))
   (define (type-name self) (string->symbol (~a self))) 
   (define (type-applicable? self) #t)
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? typed? (app get-type (== self))) v]
       [(union _ (or (== @procedure?) (== @any/c)))
        (match (union-filter v self)
          [(union (list (cons g u)))
           (assert g (argument-error caller (~a self) v))
           u]
          [u
           (assert (apply || (union-guards u)) (argument-error caller (~a self) v))
           u])]
       [_ (assert #f (argument-error caller (~a self) v))]))
   (define (type-eq? self u v)        (eq? u v))
   (define (type-equal? self u v)     (equal? u v))
   (define (type-compress self force? ps) ps)
   (define (type-construct self vs)   (car vs))
   (define (type-deconstruct self v)  (list v))]
  #:methods gen:solvable
  [(define/generic generic-solvable-default solvable-default)
   (define (solvable-default self)
     (fv self (procedure-reduce-arity
               (lambda args (generic-solvable-default (function-range self)))
               (length (function-domain self)))))
   (define (solvable-domain self) (function-domain self))
   (define (solvable-range self) (function-range self))]
  #:methods gen:custom-write
  [(define (write-proc self port m)
     (match-define (function dom ran) self)
     (for ([t dom]) (fprintf port "~a~a" t "~>"))
     (fprintf port "~a" ran))])

(define ~>
  (case-lambda
    [(d r) (function (list d) r)]
    [(d0 d1 r) (function (list d0 d1) r)]
    [(d0 d1 . rest) (function `(,d0 ,d1 ,@(drop-right rest 1)) (last rest))]))

; Represents a function value.
(struct fv (type λ)
  #:property prop:procedure
  [struct-field-index λ]
  #:methods gen:typed
  [(define (get-type self) (fv-type self))]
  #:methods gen:custom-write
  [(define (write-proc self port m)
     (fprintf port "(fv ~a)" (fv-type self)))])

(define (make-fv type proc)
  (fv type 
      (procedure-reduce-arity
       (lambda args
         (apply proc
                (for/list ([a args] [t (function-domain type)])
                  (type-cast t a))))
       (length (function-domain type)))))

(define-match-expander fv-stx
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat ...) #'(fv pat ... _)]))
  (make-variable-like-transformer #'make-fv))

(define (@fv? v)
  (match v
    [(? fv?) #t]
    [(term _ (? function?)) #t]
    [(union _ (? function?)) #t]
    [(union xs (or (== @procedure?) (== @any/c)))
     (apply || (for/list ([gv xs] #:when (@fv? (cdr gv))) (car gv)))]
    [_ #f]))
