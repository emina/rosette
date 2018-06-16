#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/lib/synthax "synthax-external.rkt")

(define-symbolic x integer?)

(define-synthax (rec x k)
  #:base (choose x (??))
  #:else (let ([smaller (rec x (sub1 k))])
           (choose smaller (+ x smaller))))

(define (h0) (??))
(define (h1 x) (choose 1 (choose x 3)))
(define (h2 x) (choose 6 (+ x (h0)) 8))
(define (h3 x) (choose 1 (h2 x)))

(define (h4 x) (rec x 0))
(define (h5 x) (rec x 1))
(define (h6 x) (rec x 2))

(define (h7 x) (choose 1 (c2 x)))
(define (h8 x) (crec x 0))
(define (h9 x) (crec x 2))
(define (h10 x)(crec (h1 x) 1)) 
  
(define (check-synth expr expected)
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
                                      (define (h3 x) (h2 x))))
    ))

(define recursive-hole-tests
  (test-suite+ "Tests for recursive holes."
    (check-unsat (synthesize #:forall x #:guarantee (assert (= (* 2 x) (h4 x)))))
    (check-synth (= -1 (h4 x)) '((define (h4 x) -1)))
    (check-synth (= -1 (h5 x)) '((define (h5 x) (let ((smaller -1)) smaller))))
    (check-synth (= -1 (h6 x)) '((define (h6 x) (let ((smaller 
                                                       (let ((smaller -1)) 
                                                         smaller)))
                                                  smaller))))
    (check-synth (= x (h4 x)) '((define (h4 x) x)))
    (check-synth (= (add1 x) (h5 x)) '((define (h5 x) (let ((smaller 1)) 
                                                        (+ x smaller)))))
    (check-synth (= (* 3 x) (h6 x)) '((define (h6 x)
                                        (let ((smaller (let ((smaller x)) 
                                                         (+ x smaller)))) 
                                          (+ x smaller)))))
  ))

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
    (check-synth (= -1 (h8 x)) '((define (c0) -1) 
                                 (define (h8 x) (c0))))
    (check-synth (= -1 (h9 x)) '((define (c0) -1) 
                                 (define (h9 x) (let ((smaller 
                                                       (let ((smaller (c0))) 
                                                         smaller)))
                                                  smaller))))
    (check-synth (= (+ x 2) (h10 x)) '((define (c0) 2)
                                       (define (h1 x) x)
                                       (define (h10 x)
                                         (let ((smaller (c0)))
                                           (+ (h1 x) smaller)))))
    ))

(module+ test
  (time (run-tests basic-tests))
  (time (run-tests recursive-hole-tests))
  (time (run-tests imported-hole-tests)))
