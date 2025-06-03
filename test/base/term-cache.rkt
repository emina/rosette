#lang racket

(require rackunit rackunit/text-ui rosette/base/core/term-cache)

(define test-ref
  (test-suite "term-cache-ref"
    (let ()
      (struct dummy (v) #:transparent)
      (define x #'x)
      ;; h should functionally be a hash-equal?
      (define h (make-term-cache
                 (list (cons 1 1) ; a number
                       (cons (list xor #t #f) #t) ; binary
                       (cons (list - 5) -5) ; unary
                       (cons x x) ; identifier
                       (cons (list + 1 2 3 4) 10) ; too many operands
                       (cons (list + 1 2 3) 6) ; ternary
                       (cons (dummy 1) (dummy 1)) ; unknown value
                       )))
      (check-equal? (term-cache-ref h 1 0) 1)
      (check-equal? (term-cache-ref h (list xor #t #f) 0) #t)
      (check-equal? (term-cache-ref h (list - 5) 0) -5)
      (check-equal? (term-cache-ref h x 0) x)
      (check-equal? (term-cache-ref h (list + 1 2 3 4) 0) 10)
      (check-equal? (term-cache-ref h (list + 1 2 3) 0) 6)
      (check-equal? (term-cache-ref h (dummy 1) 0) (dummy 1))
      (check-equal? (term-cache-ref h (dummy 2) 0) 0) ; missing
      )))

(define test-set
  (test-suite "term-cache-set"
    (let ()
      (struct dummy (v) #:transparent)
      (define x #'x)
      (define y #'y)
      ;; h should functionally be a hash-equal?
      (define h (make-term-cache
                 (list (cons 1 1) ; a number
                       (cons (list xor #t #f) #t) ; binary
                       (cons (list - 5) -5) ; unary
                       (cons x x) ; identifier
                       (cons (list + 1 2 3 4) 10) ; too many operands
                       (cons (list + 1 2 3) 6) ; ternary
                       (cons (dummy 1) (dummy 1)) ; unknown value
                       )))
      (term-cache-set! h 1 2)
      (check-equal? (term-cache-ref h 1 0) 2)

      (term-cache-set! h (list xor #t #f) #f)
      (check-equal? (term-cache-ref h (list xor #t #f) 0) #f)

      (term-cache-set! h (list xor #t #f) #f)
      (check-equal? (term-cache-ref h (list - 5) 0) -5)

      (term-cache-set! h x y)
      (check-equal? (term-cache-ref h x 0) y)

      (term-cache-set! h (list + 1 2 3 4) 11)
      (check-equal? (term-cache-ref h (list + 1 2 3 4) 0) 11)

      (term-cache-set! h (list + 1 2 3) 7)
      (check-equal? (term-cache-ref h (list + 1 2 3) 0) 7)

      (term-cache-set! h (dummy 1) (dummy 2))
      (check-equal? (term-cache-ref h (dummy 1) 0) (dummy 2))

      (term-cache-set! h (dummy 3) (dummy 3)) ; missing
      (check-equal? (term-cache-ref h (dummy 3) 0) (dummy 3))
      )))

(define test-count
  (test-suite "term-cache-count"
    (let ()
      (struct dummy (v) #:transparent)
      (define x #'x)
      ;; h should functionally be a hash-equal?
      (define h (make-term-cache
                 (list (cons 1 1) ; a number
                       (cons (list xor #t #f) #t) ; binary
                       (cons (list - 5) -5) ; unary
                       (cons x x) ; identifier
                       (cons (list + 1 2 3 4) 10) ; too many operands
                       (cons (list + 1 2 3) 6) ; ternary
                       (cons (dummy 1) (dummy 1)) ; unknown value
                       )))

      (check-equal? (term-cache-count h) 7))))

(define test-copy
  (test-suite "term-cache-copy"
    (let ()
      (struct dummy (v) #:transparent)
      (define x #'x)
      ;; h should functionally be a hash-equal?
      (define h (make-term-cache
                 (list (cons 1 1) ; a number
                       (cons (list xor #t #f) #t) ; binary
                       (cons (list - 5) -5) ; unary
                       (cons x x) ; identifier
                       (cons (list + 1 2 3 4) 10) ; too many operands
                       (cons (list + 1 2 3) 6) ; ternary
                       (cons (dummy 1) (dummy 1)) ; unknown value
                       )))

      (check-equal? (term-cache-count (term-cache-copy h)) 7))))

(define test-copy-clear
  (test-suite "term-cache-copy-clear"
    (let ()
      (struct dummy (v) #:transparent)
      (define x #'x)
      ;; h should functionally be a hash-equal?
      (define h (make-term-cache
                 (list (cons 1 1) ; a number
                       (cons (list xor #t #f) #t) ; binary
                       (cons (list - 5) -5) ; unary
                       (cons x x) ; identifier
                       (cons (list + 1 2 3 4) 10) ; too many operands
                       (cons (list + 1 2 3) 6) ; ternary
                       (cons (dummy 1) (dummy 1)) ; unknown value
                       )))

      (check-equal? (term-cache-count (term-cache-copy-clear h)) 0)
      (check-false (term-cache-weak? (term-cache-copy-clear h)))

      (define h* (make-weak-term-cache
                  (list (cons 1 1) ; a number
                        (cons (list xor #t #f) #t) ; binary
                        (cons (list - 5) -5) ; unary
                        (cons x x) ; identifier
                        (cons (list + 1 2 3 4) 10) ; too many operands
                        (cons (list + 1 2 3) 6) ; ternary
                        (cons (dummy 1) (dummy 1)) ; unknown value
                        )))
      (check-equal? (term-cache-count (term-cache-copy-clear h*)) 0)
      (check-true (term-cache-weak? (term-cache-copy-clear h*))))))

(define test-strong-weak
  (test-suite "make-term-cache + make-weak-term-cache"
    (let ()
      (struct dummy (v) #:transparent)
      (define x #'x)
      ;; h should functionally be a hash-equal?
      (define h (make-term-cache
                 (list (cons 1 1) ; a number
                       (cons (list xor #t #f) #t) ; binary
                       (cons (list - 5) -5) ; unary
                       (cons x x) ; identifier
                       (cons (list + 1 2 3 4) 10) ; too many operands
                       (cons (list + 1 2 3) 6) ; ternary
                       (cons (dummy 1) (dummy 1)) ; unknown value
                       )))

      (check-equal? (term-cache-count h) 7)
      (check-false (term-cache-weak? h))

      (define dummy-val (dummy 1))

      (set! h (make-weak-term-cache
               (list (cons 1 1) ; a number
                     (cons (list xor #t #f) #t) ; binary
                     (cons (list - 5) -5) ; unary
                     (cons x x) ; identifier
                     (cons (list + 1 2 3 4) 10) ; too many operands
                     (cons (list + 1 2 3) 6) ; ternary
                     (cons dummy-val dummy-val) ; unknown value
                     )))

      (collect-garbage)
      (collect-garbage)
      (collect-garbage)

      ;; only 1, x and, dummy-val should be left
      (check-equal? (term-cache-count h) 3)

      (set! dummy-val #f)
      (collect-garbage)
      (collect-garbage)
      (collect-garbage)

      ;; only 1 and x should be left
      (check-equal? (term-cache-count h) 2)

      (check-true (term-cache-weak? h)))))


(module+ test
  (run-tests test-ref)
  (run-tests test-set)
  (run-tests test-count)
  (run-tests test-copy)
  (run-tests test-copy-clear)
  (run-tests test-strong-weak)
  )
