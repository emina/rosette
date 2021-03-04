#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit 
         rosette/lib/synthax "synthax-external.rkt")

(define-symbolic x y integer?)

(define-synthax (rec x k)
  (assert (>= k 0))
  (choose x (??) (+ x (rec x (sub1 k)))))

(define (h0) (??))
(define (h1 x) (choose 1 (choose x 3)))
(define (h2 x) (choose 6 (+ x (h0)) 8))
(define (h3 x) (choose 1 (h2 x)))

(define-synthax (h4) (choose 1 2))
(define (h5) (h4))
(define (h6 x) (choose (??) (+ x (h5))))

(define (r1 x) (rec x 0))
(define (r2 x) (rec x 1))
(define (r3 x) (rec x 2))

(define (m1 x) (choose 1 (c2 x)))
(define (m2 x) (crec x 0))
(define (m3 x) (crec x 2))
(define (m4 x)(crec (h1 x) 1))

(define-synthax (s0 x k)
  (assert (>= k 0))
  (choose x (+ (choose 0 1) (s0 x (sub1 k)))))

(define (s1) (choose 1 2))

(define-synthax (s2 x k)
  (assert (>= k 0))
  (choose x (+ (s1) (s2 x (sub1 k)))))

(define-synthax (s3) (choose 0 1))

(define-synthax (s4 x k)
  (assert (>= k 0))
  (choose x (+ (s3) (s4 x (sub1 k)))))

(define (f0 x) (s0 x 3))
(define (f1 x) (s2 x 3))
(define (f2 x) (s4 x 5))

(define-synthax (add-one z depth)
  (assert (>= depth 0))
  (choose z (+ 1 (add-one-more z (- depth 1)))))

(define-synthax (add-one-more y depth)
  (assert (>= depth 0))
  (choose y (+ 1 (add-one y (- depth 1)))))

(define (mutually-recursive x)
  (add-one x 3))

(define-namespace-anchor tests)
(define ns (namespace-anchor->namespace tests))

(define (verified-equal? vars impl spec)
  (or (equal? impl spec)
      (begin 
        (match-define `(,_ ... (define ,spec-h ,_ ...)) spec)
        (define consts (take (map term->datum vars) (sub1 (length spec-h))))
        (define body `(let ([impl (lambda ,(cdr spec-h) ,@impl ,spec-h)]
                            [spec (lambda ,(cdr spec-h) ,@spec ,spec-h)])
                        (unsat?
                         (verify
                          (assert
                           (equal? (impl ,@consts) (spec ,@consts)))))))
        (eval body ns))))
  
(define-syntax-rule (check-synth vars expr expected)
  (let* ([vs (symbolics vars)]
         [sol (synthesize #:forall vs #:guarantee (assert expr))])
    (check-sat sol)
    (check-true
     (verified-equal? vs (map syntax->datum (generate-forms sol)) expected))))

(define basic-tests
  (test-suite+ "Basic hole tests."
    (check-synth x (= (+ 2 x) (+ (h0) x)) '((define (h0) 2)))
    (check-synth x (= x (h1 x))           '((define (h1 x) x)))
    (check-synth x (= (* 2 x) (+ (h1 x) (h1 x))) '((define (h1 x) x)))
    (check-synth x (= (+ 2 x) (h3 x)) '((define (h0) 2)
                                      (define (h2 x) (+ x (h0)))
                                      (define (h3 x) (h2 x))))
    (check-synth x (= (+ x 1) (h6 x)) '((define (h5) (let () 1))
                                      (define (h6 x) (+ x (h5)))))))


(define recursive-hole-tests
  (test-suite+ "Tests for recursive holes."
    (check-unsat (synthesize #:forall x #:guarantee (assert (= (* 2 x) (r1 x)))))
    (check-synth x (= -1 (r1 x)) '((define (r1 x)
                                   (let ((x x) (k 0))
                                     (assert (>= k 0))
                                     -1))))
    (check-synth x (= -1 (r2 x)) '((define (r2 x)
                                   (let ((x x) (k 1))
                                     (assert (>= k 0))
                                     -1))))
    (check-synth x (= -1 (r3 x)) '((define (r3 x)
                                   (let ((x x) (k 2))
                                     (assert (>= k 0))
                                     -1))))
    (check-synth x (= x (r1 x)) '((define (r1 x)
                                  (let ((x x) (k 0))
                                    (assert (>= k 0))
                                    x))))
    (check-synth x (= (add1 x) (r2 x)) '((define (r2 x)
                                         (let ((x x) (k 1))
                                           (assert (>= k 0))
                                           (+
                                            x
                                            (let ((x x) (k (sub1 k)))
                                              (assert (>= k 0))
                                              1))))))
    (check-unsat (synthesize #:forall x #:guarantee (assert (= (* 3 x) (r2 x)))))
    (check-synth x (= (* 3 x) (r3 x)) '((define (r3 x)
                                        (let ((x x) (k 2))
                                          (assert (>= k 0))
                                          (+
                                           x
                                           (let ((x x) (k (sub1 k)))
                                             (assert (>= k 0))
                                             (+
                                              x
                                              (let ((x x)
                                                    (k (sub1 k)))
                                                (assert (>= k 0))
                                                x))))))))
    (check-synth x (= (+ x 3) (mutually-recursive x))
                 '((define (mutually-recursive x)
                     (let ((z x) (depth 3))
                       (assert (>= depth 0))
                       (+
                        1
                        (let ((y z) (depth (- depth 1)))
                          (assert (>= depth 0))
                          (+
                           1
                           (let ((z y) (depth (- depth 1)))
                             (assert (>= depth 0))
                             (+
                              1
                              (let ((y z) (depth (- depth 1))) (assert (>= depth 0)) y))))))))))))

(define imported-hole-tests
  (test-suite+ "Tests that use holes defined in imported modules."
    (check-synth x (= (+ 2 x) (+ (c0) x)) '((define (c0) 2)))
    (check-synth x (= (* 2 x) (+ (c1 x) (c1 x))) '((define (c1 x) x)))
    (check-synth x (= (+ 2 x) (c3 x)) '((define (c0) 2)
                                      (define (c2 x) (+ x (c0)))
                                      (define (c3 x) (c2 x))))
    (check-synth x (= (+ 2 x) (m1 x)) '((define (c0) 2)
                                      (define (c2 x) (+ x (c0)))
                                      (define (m1 x) (c2 x))))
    (check-synth x (= -1 (m2 x)) '((define (c0) -1)
                                 (define (m2 x)
                                   (let ((x x) (k 0))
                                     (assert (>= k 0))
                                     (c0)))))
    (check-synth x (= -1 (m3 x)) '((define (c0) -1)
                                 (define (m3 x)
                                   (let ((x x) (k 2))
                                     (assert (>= k 0))
                                     (c0)))))
    (check-synth x (= (+ x 2) (m4 x)) '((define (c0) 2)
                                       (define (h1 x) x)
                                       (define (m4 x)
                                         (let ((x (h1 x)) (k 1))
                                           (assert (>= k 0))
                                           (+
                                            x
                                            (let ((x x) (k (sub1 k)))
                                              (assert (>= k 0))
                                              (c0)))))))))

(define stress-tests
  (test-suite+ "Stress tests for recursive holes."
    
    (check-synth x (= (+ x 3) (f0 x)) '((define (f0 x)
                                        (let ((x x) (k 3))
                                          (assert (>= k 0))
                                          (+
                                           1
                                           (let ((x x) (k (sub1 k)))
                                             (assert (>= k 0))
                                             (+
                                              1
                                              (let ((x x)
                                                    (k (sub1 k)))
                                                (assert (>= k 0))
                                                (+
                                                 1
                                                 (let ((x x)
                                                       (k (sub1 k)))
                                                   (assert (>= k 0))
                                                   x))))))))))
    (check-synth x (= (+ x 3) (f1 x)) '((define (s1) 1)
                                      (define (f1 x)
                                        (let ((x x) (k 3))
                                          (assert (>= k 0))
                                          (+
                                           (s1)
                                           (let ((x x) (k (sub1 k)))
                                             (assert (>= k 0))
                                             (+
                                              (s1)
                                              (let ((x x)
                                                    (k (sub1 k)))
                                                (assert (>= k 0))
                                                (+
                                                 (s1)
                                                 (let ((x x)
                                                       (k (sub1 k)))
                                                   (assert (>= k 0))
                                                   x))))))))))
    (check-synth x (= (+ x 5) (f2 x)) '((define (f2 x)
                                        (let ((x x) (k 5))
                                          (assert (>= k 0))
                                          (+
                                           (let () 1)
                                           (let ((x x) (k (sub1 k)))
                                             (assert (>= k 0))
                                             (+
                                              (let () 1)
                                              (let ((x x) (k (sub1 k)))
                                                (assert (>= k 0))
                                                (+
                                                 (let () 1)
                                                 (let ((x x) (k (sub1 k)))
                                                   (assert (>= k 0))
                                                   (+
                                                    (let () 1)
                                                    (let ((x x) (k (sub1 k)))
                                                      (assert (>= k 0))
                                                      (+
                                                       (let () 1)
                                                       (let ((x x) (k (sub1 k))) (assert (>= k 0)) x))))))))))))))))

(define-synthax (op left right)
 [choose (+ left right)
         (- left right)])

(define-synthax (idx tid step depth)
  (assert (>= depth 0)) 
  (choose tid step  
          (op (idx tid step (- depth 1))
              (idx tid step (- depth 1)))))

(define (cidx1 tid step)
  (idx tid step 1))

(define regression-tests
  (test-suite+ "Regression tests for synthax."

    (check-synth (list x y) (= x (cidx1 x y))
                 '((define (cidx1 tid step)
                     (let ((tid tid) (step step) (depth 1)) (assert (>= depth 0)) tid))))
    (check-synth (list x y) (= (+ x y) (cidx1 x y))
                 '((define (cidx1 tid step)
                     (let ((tid tid) (step step) (depth 1))
                       (assert (>= depth 0))
                       (let ((left
                              (let ((tid tid) (step step) (depth (- depth 1)))
                                (assert (>= depth 0))
                                tid))
                             (right
                              (let ((tid tid) (step step) (depth (- depth 1)))
                                (assert (>= depth 0))
                                step)))
                         (+ left right))))))))
               
(module+ test
  (time (run-tests basic-tests))
  (time (run-tests recursive-hole-tests))
  (time (run-tests imported-hole-tests))
  (time (run-tests stress-tests))
  (time (run-tests regression-tests)))
