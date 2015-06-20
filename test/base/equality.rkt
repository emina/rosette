#lang racket

(require rackunit rackunit/text-ui
         rosette/base/equality 
         rosette/base/term
         rosette/base/bool
         rosette/base/box
         rosette/base/num 
         rosette/base/procedure
         rosette/base/enum
         rosette/base/list
         rosette/base/vector
         (only-in rosette/base/struct struct)
         rosette/base/merge
         (only-in rosette/base/define define-symbolic))

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

(define (primitive-equality-tests =?)
  (check-equal? (=? x x) #t)
  (check-equal? (=? a a) #t)
  (check-equal? (=? x y) (@= x y))
  (check-equal? (=? a b) (<=> a b))
  (check-equal? (=? x a) #f)
  (check-equal? (=? x 1) (@= x 1))
  (check-equal? (=? a #t) a)
  (check-equal? (=? a #f) (! a))
  (check-equal? (=? 1 1) #t)
  (check-equal? (=? 1 0) #f)
  (check-equal? (=? #t #t) #t)
  (check-equal? (=? #f #f) #t)
  (check-equal? (=? #t #f) #f)
  (check-equal? (=? (@+ x y) (@+ x y)) #t))

(define (box-equality-tests @box =?)
  (check-equal? (=? (@box 1) (@box 1)) #t)
  (check-equal? (=? (@box x) (@box x)) #t)
  (check-equal? (=? (@box x) (@box y)) (@= x y))
  (check-equal? (=? (@box a) (@box y)) #f)
  (check-equal? (=? (@box a) (@box b)) (@equal? a b))
  (check-equal? (=? (@box (@box a)) (@box (@box b))) (@equal? a b)))
  
(define (transparent-immutable-tests pair =?)
  (check-equal? (=? (pair 1 4) (pair 1 4)) #t)
  (check-equal? (=? (pair 1 x) (pair 1 x)) #t)
  (check-equal? (=? (pair x 1) (pair x 1)) #t)
  (check-equal? (=? (pair y z) (pair y z))  #t)
  
  (check-equal? (=? (pair x y) (pair y x)) (@= x y))
  (check-equal? (=? (pair x a) (pair y #t)) (&& (@= x y) a))
  (check-equal? (=? (pair x y) (pair 2 3)) (&& (@= x 2) (@= y 3)))
  (check-equal? (=? (pair x 3) (pair 2 y)) (&& (@= x 2) (@= y 3)))
  (check-equal? (=? (pair a b) (pair b a)) (@equal? a b))
  (check-equal? (=? (pair (pair x y) z) (pair (pair 2 3) 5)) 
                (&& (&& (@= x 2) (@= y 3)) (@= z 5))))

(define (sequence-equality-tests seq =?)
  (check-equal? (=? (seq 1 4 7 9) (seq 1 4 7 9)) #t)
  (check-equal? (=? (seq 1 x 7 9) (seq 1 x 7 9)) #t)
  (check-equal? (=? (seq 1 x 7 a) (seq 1 x 7 a)) #t)
  (check-equal? (=? (seq x y z) (seq x y z))  #t)
  (check-equal? (=? (seq x a y b z c) (seq x a y b z c)) #t) 
  
  (check-equal? (=? (seq 1 4 5 9) (seq 1 4 7 9)) #f)
  (check-equal? (=? (seq 1 x 7 5) (seq 1 x 7 9)) #f)
  (check-equal? (=? (seq 5 x 7 a) (seq 1 x 7 a)) #f)
  
  (check-equal? (=? (seq x y) (seq y x)) (@= x y))
  (check-equal? (=? (seq x y a) (seq y x #t)) (&& (@= x y) a))
  (check-equal? (=? (seq x y z) (seq 2 3 5)) (&& (@= x 2) (@= y 3) (@= z 5)))
  (check-equal? (=? (seq x 3) (seq 2 y)) (&& (@= x 2) (@= y 3)))
  (check-equal? (=? (seq a b) (seq b a)) (@equal? a b))
  (check-equal? (=? (seq (seq x y) z) (seq (seq 2 3) 5)) 
                (&& (&& (@= x 2) (@= y 3)) (@= z 5))))

(define (struct-equal?-tests)
  (check-equal? (@equal? (i0 1) (i0 1)) (equal? (i0 1) (i0 1)))
  (check-equal? (@equal? (i0 x) (i0 x)) (equal? (i0 x) (i0 x)))
  (check-equal? (@equal? (i0 x) (i0 y)) (@= x y))
  (check-equal? (@equal? (i0 1) (i1 1 2)) (equal? (i0 1) (i1 1 2)))
  (check-equal? (@equal? (i0 1) (i2 1 2)) (equal? (i0 1) (i2 1 2)))
  (check-equal? (@equal? (i1 1 2) (i2 1 2)) (equal? (i1 1 2) (i2 1 2)))
  (check-equal? (@equal? (q0 1) (q0 1)) (equal? (q0 1) (q0 1)))
  (check-equal? (@equal? (p2 1 2) (p2 1 2)) (equal? (p2 1 2) (p2 1 2)))
  (check-equal? (@equal? (h1 1 2) (h2 1)) #t)
  (check-equal? (@equal? (h1 1 2) (h2 x)) (@= x 1))
  (check-equal? (@equal? (h1 1 2) (h0 1)) #t)
  (check-equal? (@equal? (h1 1 2) (h0 x)) (@= x 1))
  (check-equal? (@equal? (h1 1 x) (h1 y 2)) (@= y 1)))

(define (struct-eq?-tests)
  (check-equal? (@eq? (i0 1) (i0 1)) #t)
  (check-equal? (@eq? (i0 x) (i0 x)) #t)
  (check-equal? (@eq? (i0 x) (i0 y)) (@= x y))
  (check-equal? (@eq? (i0 1) (i1 1 2)) #f)
  (check-equal? (@eq? (i0 1) (i2 1 2)) #f)
  (check-equal? (@eq? (i1 1 2) (i2 1 2)) #f)
  (check-equal? (@eq? (q0 1) (q0 1)) #f)
  (check-equal? (@eq? (p2 1 2) (p2 1 2)) #t)
  (check-equal? (@eq? (h1 1 2) (h2 1)) #f)
  (check-equal? (@eq? (h1 1 2) (h2 x)) #f)
  (check-equal? (@eq? (h1 1 2) (h0 1)) #f)
  (check-equal? (@eq? (h1 1 2) (h0 x)) #f)
  (check-equal? (@eq? (h1 1 x) (h1 y 2)) #f))

(define (opaque-or-mutable-tests constructor)
  (check-false (@eq? (constructor 1) (constructor 1)))
  (check-false (@eq? (@list (constructor 1)) (@list (constructor 1))))
  (check-false (@eq? (@vector-immutable (constructor 1)) (@vector-immutable (constructor 1))))
  (check-false (@eq? (cons (constructor 1) (constructor 2)) (cons (constructor 1) (constructor 2))))
  (check-false (@eq? (@box-immutable (constructor 1)) (@box-immutable (constructor 1))))
  (check-false (@eq? (i0 (constructor 1)) (i0 (constructor 1))))
  (let ([v (constructor 1)])
    (check-true (@eq? v v))
    (check-true (@eq? (@list v) (@list v)))
    (check-true (@eq? (@vector-immutable v) (@vector-immutable v)))
    (check-true (@eq? (@cons v v) (@cons v v)))
    (check-true (@eq? (@box-immutable v) (@box-immutable v)))
    (check-true (@eq? (i0 v) (i0 v)))))

(define (union-equality-tests)
  (check-equal? (@eq? (merge a b x) (merge a b x)) #t)
  (check-equal? (@equal? (merge a b x) (merge a b x)) #t)
  (check-equal? (@eq? (merge a b (@list x)) (merge a b (@list x))) #t)
  (check-equal? (@eq? (merge a b (@vector x)) (merge a b (@vector x))) a)
  (check-equal? (@equal? (merge a b (@list x)) (merge a b (@list x))) #t)
  (check-equal? (@eq? (merge a b x) (merge c y a))
                (|| (&& a (! c) (@equal? b a)) 
                    (&& (! a) c (@equal? x y)))))

(define (struct-hash-tests)
  (check-equal? (equal-hash-code (h0 1)) (equal-hash-code (h0 1))))

(define equality-tests
  (test-suite 
   "Tests for rosette/base/equality.rkt"
   #:before (lambda () (printf "Testing rosette/base/equality.rkt\n"))
   
   ; eq? and equal? behave the same on transparent immutable values
   (primitive-equality-tests @eq?)
   (primitive-equality-tests @equal?)
   (box-equality-tests @box-immutable @eq?)
   (box-equality-tests @box-immutable @equal?)
   (transparent-immutable-tests @cons @eq?)
   (transparent-immutable-tests @cons @equal?)
   (transparent-immutable-tests i1 @eq?)
   (transparent-immutable-tests i1 @equal?)
   (transparent-immutable-tests p2 @eq?)
   (transparent-immutable-tests p2 @equal?)
   (sequence-equality-tests @list @eq?)
   (sequence-equality-tests @list @equal?)
   (sequence-equality-tests @vector-immutable @eq?)
   (sequence-equality-tests @vector-immutable @equal?)
   ; equal? tests should also pass on transparent mutable objects
   (box-equality-tests @box @equal?)
   (sequence-equality-tests @vector @equal?)
   (transparent-immutable-tests m2 @equal?)
   ; opaque / mutable / union tests
   (opaque-or-mutable-tests @box)
   (opaque-or-mutable-tests @vector)
   (opaque-or-mutable-tests (lambda args (lambda () args)))
   (opaque-or-mutable-tests q0)
   (opaque-or-mutable-tests m0)
   (opaque-or-mutable-tests h0)
   (union-equality-tests)
   ; struct tests
   (struct-equal?-tests)
   (struct-eq?-tests)
   (struct-hash-tests)))

(time (run-tests equality-tests))
