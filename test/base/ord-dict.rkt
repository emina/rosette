#lang racket

(require rackunit rackunit/text-ui  rosette/lib/roseunit
         rosette/base/util/ord-dict)


(define (check-dict d eq k0 k1 k2)
  (check-equal? (dict-count d) 0)
  (dict-set! d k0 'a)
  (check-equal? (dict-count d) 1)
  (check-true (dict-has-key? d k0))
  (check-false (dict-has-key? d k1))
  (check-exn exn:fail? (thunk (dict-ref d k1)))
  (dict-set! d k1 'b)
  (dict-set! d k2 'c)
  (check-equal? (dict-count d) 3)
  (check-equal? (dict-iterate-first d) (list k2 k1 k0))
  (check-equal? (dict-iterate-next d (dict-iterate-first d)) (list k1 k0))
  (check-equal? (dict-iterate-next d (dict-iterate-next d (dict-iterate-first d))) (list k0))
  (check-equal? (dict-iterate-next d (dict-iterate-next d (dict-iterate-next d (dict-iterate-first d)))) #f)
  (check-equal? (dict-iterate-key d (list k2 k1 k0)) k2)
  (check-equal? (dict-iterate-value d (list k2 k1 k0)) 'c)
  (check-equal? (dict-iterate-key d (list k1 k0)) k1)
  (check-equal? (dict-iterate-value d (list k1 k0)) 'b)
  (check-equal? (dict-iterate-key d (list k0)) k0)
  (check-equal? (dict-iterate-value d (list k0)) 'a)
  (check-exn exn:fail? (thunk (dict-iterate-key d '())))
  (dict-remove! d k1)
  (check-equal? (dict->list d) `((,k2 . c) (,k0 . a)))
  (dict-remove! d k1)
  (check-equal? (dict->list d) `((,k2 . c) (,k0 . a)))
  (dict-set! d k1 'b)
  (check-equal? (dict->list d) `((,k1 . b) (,k2 . c) (,k0 . a)))
  (dict-set! d k2 'd)
  (check-equal? (dict->list d) `((,k1 . b) (,k2 . d) (,k0 . a)))
  )
  
(define tests-dict-equal?
  (test-suite+
   "Tests for equal? ordered dictionaries in ord-dict.rkt"
   (check-dict (odict) equal? 1 2 3)))
  
(define tests-dict-eq?
  (test-suite+
   "Tests for eq? ordered dictionaries in ord-dict.rkt"
   (check-dict (odict null eq?) eq?
               (vector 0) (vector 0) (vector 0))))
       
(module+ test
  (time (run-tests tests-dict-equal?))
  (time (run-tests tests-dict-eq?)))
