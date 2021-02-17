#lang rosette

(define (sum0 xs) (foldl + xs))
(define (sum1 xs) (foldl + 0 xs))

(define (sum2 xs)
  (cond
    [(null? xs) 0]
    [(null? (cdr xs)) (car xs)] 
    [(andmap (curry = (car xs)) (cdr xs))  
     (* (length xs) (cdr xs))] 
    [else (apply + xs)]))

(define-symbolic xs integer? #:length 4)

(require rackunit)

(define (tests0 sum)
  
  (define (post xs)
    (assert (= (sum xs) (sum (filter-not zero? xs)))))

  (define (query xs)
    (verify (post xs)))
   
  (test-suite
   "An example suite for a sum query."
   #:before clear-vc!
   #:after  clear-vc!

   (test-case
    "Test sum with concrete values."
    (check = (sum '()) 0)
    (check = (sum '(-1)) -1)
    (check = (sum '(-2 2)) 0)
    (check = (sum '(-1 0 3)) 2))
     
   (test-case
    "Test query post for exceptions."
    (before
     (clear-vc!)
     (check-not-exn (thunk (post xs)))))
  
   (test-case
    "Test query outcome."
    (before
     (clear-vc!)
     (check-pred unsat? (query xs))))))


(define (tests1 sum)

  (define (pre xs)
    (assume (positive? (sum xs))))
  
  (define (post xs)
    (assert (ormap positive? xs)))

  (define (query xs)
    (pre xs)
    (verify (post xs)))
   
  (test-suite
   "An example suite for a sum query."
   #:before clear-vc!
   #:after  clear-vc!

   (test-case
    "Test sum with concrete values."
    (check = (sum '()) 0)
    (check = (sum '(-1)) -1)
    (check = (sum '(-2 2)) 0)
    (check = (sum '(-1 0 3)) 2))

   (test-case
    "Test query post for exceptions."
    (before
     (clear-vc!)
     (check-not-exn (thunk (pre xs)))))
      
   (test-case
    "Test query post for exceptions."
    (before
     (clear-vc!)
     (check-not-exn (thunk (post xs)))))
  
   (test-case
    "Test query outcome."
    (before
     (clear-vc!)
     (check-pred unsat? (query xs))))))

(run-test (tests0 sum0))
(run-test (tests0 sum1))
(run-test (tests0 sum2))
(run-test (tests1 sum2))
