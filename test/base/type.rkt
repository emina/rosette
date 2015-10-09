#lang racket

(require rackunit rackunit/text-ui
         rosette/base/core/type
         rosette/base/core/equality 
         rosette/base/core/term
         rosette/base/adt/procedure
         rosette/base/core/bool
         rosette/base/adt/box
         rosette/base/core/num 
         rosette/base/struct/enum
         rosette/base/adt/list
         rosette/base/adt/vector
         rosette/base/struct/struct
         rosette/base/core/any
         rosette/base/core/merge
         (only-in rosette/base/form/define define-symbolic))

(define-symbolic x @number?)
(define-symbolic y @number?)
(define-symbolic z @number?)

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

; transparent immutable structs 
(struct i0 (x) #:transparent)
(struct i1 i0 (y) #:inspector #f)
(struct i2 i1 () #:transparent)

; opaque immutable structs
(struct q0 (x))
(struct q1 q0 (y) #:transparent)

; procedure structs
(struct p0 (p) #:property prop:procedure (lambda (self) (p0-p self)))
(struct p1 p0 ())
(struct p2 i0 (p) #:transparent #:property prop:procedure (lambda (self) (p2-p self)))
(struct p3 p0 ())

; equal+hash structs
(struct h0 (x)
  #:methods gen:equal+hash
  [(define (equal-proc a b rec) (rec (h0-x a) (h0-x b)))
   (define (hash-proc a rec) (rec (h0-x a)))
   (define (hash2-proc a rec) (rec (h0-x a)))])

(struct h1 h0 (y))
(struct h2 h0 ())

; mutable structs
(struct m0 (m) #:mutable #:transparent)
(struct m1 p2 ([m #:mutable]))
(struct m2 i0 (m) #:mutable #:transparent)

; enums
(define-enum e (list 1 #t #f 4 identity "foo" 'bar))

(define (least-common-supertype-tests)
  (for ([t types])
    (check-eq? (least-common-supertype t t) t)
    (check-eq? (least-common-supertype t @any?) @any?))
  (check-eq? (least-common-supertype @list? @pair?) @pair?)
  (check-eq? (least-common-supertype @procedure? @pair?) @any?)
  (check-eq? (least-common-supertype p1? p2?) @procedure?)
  (check-eq? (least-common-supertype p1? p0?) p0?)
  (check-eq? (least-common-supertype p1? p3?) p0?)
  (check-eq? (least-common-supertype h1? h2?) h0?)
  (check-eq? (least-common-supertype h1? p1?) @any?)
  )

(define (subtype?-tests)
  (let loop ([types types])
    (match types
      [(list t rest ...)
       (for ([r rest])
         (check-false (subtype? r t)))]
      [_ (void)]))
  (for ([t types]) 
    (check-true (subtype? t @any?)))
  (check-true (subtype? @list? @pair?))
  (check-true (subtype? i0? @any?))
  (check-true (subtype? p1? @procedure?))
  (check-true (subtype? e? e?))
  (check-true (subtype? e? @any?))
  (check-true (subtype? i0? i0?))
  (check-true (subtype? i1? i0?))
  (check-true (subtype? i2? i0?))
  (check-true (subtype? i2? i1?)))

(define type-tests
  (test-suite 
   "Tests for rosette types"
   #:before (lambda () (printf "Testing rosette types\n"))
   (least-common-supertype-tests)
   (subtype?-tests)))

(time (run-tests type-tests))
