#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)


(define (make-even n)
  (for/vector #:length n ([i n]) (* 2 i)))

(define (make-odd n)
  (for/vector #:length n ([i n]) (add1 (* 2 i))))

(define (check-copy! dest dest-start src src-start src-dest expected)
  (vector-copy! dest dest-start src src-start src-dest)
  (check-equal? dest expected))

(define vector-copy!-errors
  (test-suite+
   "Tests for vector-copy! errors"
   
   (let ([x (make-even 5)]
         [y (make-odd 4)])
     (define-symbolic b0 boolean?)
     (check-exn exn:fail? (thunk (vector-copy! x 5 y)))
     (check-exn exn:fail? (thunk (vector-copy! x 0 y 5)))
     (check-exn exn:fail? (thunk (vector-copy! x 0 y 3 5)))
     (check-exn exn:fail? (thunk (vector-copy! y 0 x)))
     (check-exn exn:fail? (thunk (vector-copy! x 4 y)))
     (check-exn exn:fail? (thunk (vector-copy! (if b0 x y) 4 y))))))

(define vector-copy!-tests
  (test-suite+
   "Tests for vector-copy!"
   
   (local [(define-symbolic b0 b1 b2 b3 boolean?)
           (define-symbolic dest-start src-start src-end integer?)
           (define len (- src-end src-start))]
     
     (let ([x (vector 1 2 3 4 5)])
       (check-copy! x 0 x 0 0 (vector 1 2 3 4 5))
       (check-copy! x 0 x 0 5 (vector 1 2 3 4 5))
       (check-copy! x 0 x 1 5 (vector 2 3 4 5 5))
       (check-copy! x 4 x 0 1 (vector 2 3 4 5 2)))
     
     (check-copy! (vector 1 2 3 4) 2 (vector 5 6) 1 2 (vector 1 2 6 4))
     (check-copy! (vector 1 2 3 4) dest-start (vector 5 6 7 8) 0 4 
                  (vector (if (= 0 dest-start) 5 1) 
                          (if (= 0 dest-start) 6 2) 
                          (if (= 0 dest-start) 7 3) 
                          (if (= 0 dest-start) 8 4)))
     (check-copy! (vector 1 2 3 4) 2 (vector 5 6 7) src-start (+ src-start 2)
                  (vector 1 2 
                          (if (= 1 src-start) 6 (if (= 0 src-start) 5 3)) 
                          (if (= 1 src-start) 7 (if (= 0 src-start) 6 4))))
     (check-copy! (vector 1) dest-start (vector 2) src-start src-end
                  (vector (if (&& (= 1 len) (= 0 src-start) (= 0 dest-start)) 2 1)))
     (check-copy! (vector 1 3) dest-start (vector 2) src-start src-end
                  (vector (if (&& (= 1 len) (= 0 src-start) (= 0 dest-start)) 2 1)
                          (if (&& (= 1 len) (= 0 src-start) (= 1 dest-start)) 2 3)))
     (check-copy! (vector 1) dest-start (vector 2 3) src-start src-end
                  (vector (if (&& (= 1 len) (= 1 src-start) (= 0 dest-start)) 3  
                              (if (&& (= 1 len) (= 0 src-start) (= 0 dest-start)) 2 1))))
     (check-copy! (vector 1 2) dest-start (vector 3 4) src-start src-end
                  (vector (if (&& (= 2 len) (= 0 src-start) (= 0 dest-start)) 3 
                              (if (&& (= 1 len) (= 1 src-start) (= 0 dest-start)) 4 
                                  (if (&& (= 1 len) (= 0 src-start) (= 0 dest-start)) 3 1)))
                          (if (&& (= 2 len) (= 0 src-start) (= 0 dest-start)) 4 
                              (if (&& (= 1 len) (= 1 src-start) (= 1 dest-start)) 4 
                                  (if (&& (= 1 len) (= 0 src-start) (= 1 dest-start)) 3 2)))))
     (let ([x (vector 1 2)]
           [y (vector 3)])
       (vector-copy! (if b0 x y) dest-start (vector 5) src-start src-end)
       (check-equal? x (vector (if b0 (if (&& (= 1 len) (= 0 src-start) (= 0 dest-start)) 5 1) 1)
                               (if b0 (if (&& (= 1 len) (= 0 src-start) (= 1 dest-start)) 5 2) 2)))
       (check-equal? y (vector (if b0 3 (if (&& (= 1 len) (= 0 src-start) (= 0 dest-start)) 5 3)))))
     
     )))
   
(module+ test
  (time (run-tests vector-copy!-errors))
  (time (run-tests vector-copy!-tests)))
