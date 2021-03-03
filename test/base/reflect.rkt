#lang rosette


(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic a b boolean?)
(define-symbolic x y z integer?)

(struct triple (one two three) #:mutable #:transparent)


(define (io)
  (define b1 (box 1))
  (define bx (box x))
  (define v1 (vector 4 (list 3 2 y) bx))
  (define v2 (vector 5 (cons (triple bx b1 6) (vector (box z) 7 (triple y x z)))))
  (define b2 (box 0))
  (define v3 (vector (cons (triple b2 b1 6) (vector (box z) 7 (triple y x z)))))
  (set-box! b2 v3)
  (define b3 (box 0))
  (define v4 (triple (triple b3 8 9) (box 11) (list (vector 3) 5)))
  (set-box! b3 v4)
  (list (cons b1 null)
        (cons #t null)
        (cons 4 null)
        (cons (list 1 2 (vector 5 (triple 6 7 (box 8)))) null)
        (cons bx (list x))
        (cons y (list y))
        (cons (list x y z) (list x y z))
        (cons v1 (list y x))
        (cons v2 (list x z y))
        (cons v3 (list z y x))
        (cons v4 null)
        (cons (if a (if b v4 b3) v1) (list a b y x))))

(define tests:symbolics
  (test-suite+
   "Tests for symbolics in rosette/base/core/reflect."
   (for ([kv (io)])
     (check-equal? (symbolics (car kv)) (cdr kv)))))

(define tests:concrete?
  (test-suite+
   "Tests for concrete? in rosette/base/core/reflect."
   (for ([kv (io)])
     (check-equal? (concrete? (car kv)) (null? (cdr kv))))))

(define tests:symbolic?
  (test-suite+
   "Tests for symbolic? in rosette/base/core/reflect."
   (for ([kv (io)])
     (check-equal? (symbolic? (car kv)) (not (null? (cdr kv)))))))

(module+ test
  (time (run-tests tests:symbolics))
  (time (run-tests tests:concrete?))
  (time (run-tests tests:symbolic?)))