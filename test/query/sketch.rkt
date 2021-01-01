#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/lib/sketch "sketch-external.rkt")

(define-symbolic x integer?)


(define (h0) (??))
(define (h1 x) (choose 1 (choose x 3)))
(define (h2 x) (choose 6 (+ x (h0)) 8))
(define (h3 x) (choose 1 (h2 x)))

(define (h7 x) (choose 1 (c2 x)))

(define-syntax-rule (check-synth expr expected)
  (let ([sol (synthesize #:forall x #:guarantee (assert expr))])
    (check-sat sol)
    (check-equal? (list->set (map syntax->datum (generate-forms sol)))
                  (list->set expected))))

(define basic-tests
  (test-suite+ "Basic hole tests."
    (check-synth (= (+ 2 x) (+ (h0) x)) '((define (h0) 2)))
    (check-synth (= (* 2 x) (+ (h1 x) (h1 x))) '((define (h1 x) x)))
    (check-synth (= (+ 2 x) (h3 x)) '((define (h0) 2)
                                      (define (h2 x) (+ x (h0)))
                                      (define (h3 x) (h2 x))))))


(define imported-hole-tests
  (test-suite+ "Tests that use holes defined in imported modules."
    (check-synth (= (+ 2 x) (+ (c0) x)) '((define (c0) 2)))
    (check-synth (= (* 2 x) (+ (c1 x) (c1 x))) '((define (c1 x) x)))
    (check-synth (= (+ 2 x) (c3 x)) '((define (c0) 2)
                                      (define (c2 x) (+ x (c0)))
                                      (define (c3 x) (c2 x))))
    (check-synth (= (+ 2 x) (h7 x)) '((define (c0) 2)
                                      (define (c2 x) (+ x (c0)))
                                      (define (h7 x) (c2 x))))
    ))

(module+ test
  (time (run-tests basic-tests))
  (time (run-tests imported-hole-tests)))
