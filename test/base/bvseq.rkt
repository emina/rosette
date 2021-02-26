#lang rosette

(require rackunit rackunit/text-ui (rename-in rackunit [check-exn rackunit/check-exn])
         rosette/lib/roseunit (only-in rosette/base/core/merge merge*))

(define-symbolic b2 (bitvector 2))
(define-symbolic b3 (bitvector 3))

(define-symbolic a c d e boolean?)

(define b2∪3 (if a b2 b3))
(define b2∪3∪Err (if c b2∪3 'err))

(define-syntax-rule (check-exn e ...)
  (begin
    (rackunit/check-exn e ...)
    (clear-vc!)))

(define-syntax-rule (check≡ actual expected prop ...)
  (let ([ra (with-vc (call-with-values (lambda () actual) list))]
        [re (with-vc (call-with-values (lambda () expected) list))])
    (check-pred unsat?
                (verify (assert (equal? (vc-assumes (result-state ra))
                                        (vc-assumes (result-state re))))))
    ;(printf "VC:~a\nActual: ~a\nExpected: ~a\n" (vc) (result-state ra) (result-state re))
    ;(printf "Actual: ~a\nExpected: ~a\n" ra re)
    (check-pred unsat?
                (verify (assert (equal? (vc-asserts (result-state ra))
                                        (vc-asserts (result-state re))))))
    (check-pred unsat?
                (verify (begin (assume (&& (vc-assumes (result-state ra)) (vc-asserts (result-state ra))))
                               (assert (equal? (result-value ra) (result-value re))))))
    (check-pred unsat?
                (verify (begin (assume (&& (vc-assumes (result-state ra)) (vc-asserts (result-state ra))))
                               (assert prop)))) ...))

(define (check-length-bv length-bv length make)
  (define (spec xs t)
    (integer->bitvector (length xs) t))

  (define t3 (bitvector 3))
  (define t4 (bitvector 4))
  (define t3∪4 (if a t3 t4))
  (define t3∪4∪Err (if c t3∪4 'bad))
  
  (define (checks vs)
    (check≡ (length-bv xs t4) (spec xs t4))
    (check≡ (length-bv xs t3∪4) (spec xs t3∪4))
    (check≡ (length-bv xs t3∪4∪Err) (spec xs t3∪4∪Err)))
  
  (define xs (make 1 2 3 4 5))
  (define ys (if d xs (make 6 7 8 9 10 11 12)))

  (checks xs)
  (checks ys)
  (checks (if e ys (make)))
  (checks (if e ys 'bad)))

(define (checks impl spec vs . args)
  (check≡ (apply impl vs (bv 2 4) args) (apply spec vs (bv 2 4) args))
  (check≡ (apply impl vs b2 args) (apply spec vs b2 args))
  (check≡ (apply impl vs b3 args) (apply spec vs b3 args))
  (check≡ (apply impl vs b2∪3 args) (apply spec vs b2∪3 args))
  (check≡ (apply impl vs b2∪3∪Err args) (apply spec vs b2∪3∪Err args)))

(define (check-ref-bv ref-bv ref make)
  (define (spec xs idx)
    (ref xs (bitvector->natural idx)))
  
  (check-exn exn:fail? (thunk (ref-bv (make) (bv 0 2))))
  (check-exn exn:fail? (thunk (ref-bv (if a (make) 'foo) (bv 0 2))))

  (define xs (make 1 2 3 4 5))
  (define ys (if d xs (make 6 7 8 9 10 11 12)))

  (checks ref-bv spec xs)
  (checks ref-bv spec ys)
  (checks ref-bv spec (if e ys (make)))
  (checks ref-bv spec (if e ys 'bad)))

(define (check-get-bv get-bv get)
  (define (spec xs pos)
    (get xs (bitvector->natural pos)))

  (define xs (list 1 2 3 4 5))
  (define ys (if d xs (list 6 7 8 9 10 11 12)))

  (checks get-bv spec (list))
  (checks get-bv spec xs)
  (checks get-bv spec ys)
  (checks get-bv spec (if e ys (list)))
  (checks get-bv spec (if e ys 'bad)))

(define (check-split-bv split-bv split)
  (define (spec xs pos)
    (split xs (bitvector->natural pos)))

  (define xs (list 1 2 3 4 5))
  (define ys (if d xs (list 6 7 8 9 10 11 12)))

  (checks split-bv spec (list))
  (checks split-bv spec xs)
  (checks split-bv spec ys)
  (checks split-bv spec (if e ys (list)))
  (checks split-bv spec (if e ys 'bad)))

(define (check-list-set-bv)
  (define (spec xs idx v)
    (list-set xs (bitvector->natural idx) v))

  (define xs (list 1 2 3 4 5))
  (define ys (if d xs (list 6 7 8 9 10 11 12)))

  (checks list-set-bv spec (list) 15)
  (checks list-set-bv spec xs 15)
  (checks list-set-bv spec ys 15)
  (checks list-set-bv spec (if e ys (list)) 15)
  (checks list-set-bv spec (if e ys 'bad) 15))

(define (check-vector-set!-bv)
  (define (spec xs idx v)
    (vector-set! xs (bitvector->natural idx) v))

  (define (check vs idx [v 15])
    (define v0 (vs))
    (define v1 (vs))
    (check≡ (vector-set!-bv v0 idx v) (spec v1 idx v) (equal? v0 v1)))
     
  (define (checks vs [v 15])
    (check vs (bv 2 4) v)
    (check vs b2 v)
    (check vs b3 v)
    (check vs b2∪3 v)
    (check vs b2∪3∪Err v))

  (define (xs) (vector 1 2 3 4 5))
  (define (ys) (if d (xs) (vector 6 7 8 9 10 11 12)))

  (checks vector)
  (checks xs)
  (checks ys)
  (checks (lambda () (if e (ys) (vector))))
  (checks (lambda () (if e (ys) 'bad))))

  

(define tests:list-ref-bv
  (test-suite+
   "Tests for list-ref-bv in rosette/base/adt/bvseq.rkt"
   (check-ref-bv list-ref-bv list-ref list)))

(define tests:list-set-bv
  (test-suite+
   "Tests for list-set-bv in rosette/base/adt/bvseq.rkt"
   (check-list-set-bv)))

(define tests:list-get-bv
  (test-suite+
   "Tests for take-bv, take-right-bv, drop, drop-right, and list-tail in rosette/base/adt/bvseq.rkt"
   (check-get-bv take-bv take)
   (check-get-bv take-right-bv take-right)
   (check-get-bv drop-bv drop)
   (check-get-bv drop-right-bv drop-right)
   (check-get-bv list-tail-bv list-tail)))

(define tests:list-split-bv
  (test-suite+
   "Tests for split-at-bv and split-at-right-bv in rosette/base/adt/bvseq.rkt"
   (check-split-bv split-at-bv split-at)
   (check-split-bv split-at-right-bv split-at-right)))

(define tests:length-bv
  (test-suite+
   "Tests for length-bv in rosette/base/adt/bvseq.rkt"
   (check-length-bv length-bv length list)))

(define tests:vector-ref-bv
  (test-suite+
   "Tests for vector-ref-bv in rosette/base/adt/bvseq.rkt"
   (check-ref-bv vector-ref-bv vector-ref vector)))

(define tests:vector-set!-bv
  (test-suite+
   "Tests for vector-set!-bv in rosette/base/adt/bvseq.rkt"
   (check-vector-set!-bv)))

(define tests:vector-length-bv
  (test-suite+
   "Tests for vector-length-bv in rosette/base/adt/bvseq.rkt"
   (check-length-bv vector-length-bv vector-length vector)))

(module+ test
  (time (run-tests tests:list-ref-bv))
  (time (run-tests tests:list-set-bv))
  (time (run-tests tests:list-get-bv))
  (time (run-tests tests:list-split-bv))
  (time (run-tests tests:length-bv))
  (time (run-tests tests:vector-ref-bv))
  (time (run-tests tests:vector-set!-bv))
  (time (run-tests tests:vector-length-bv)))