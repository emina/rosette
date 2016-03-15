#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define (minimize obj . asserts)
  (optimize #:minimize (if (list? obj) obj (list obj))
            #:guarantee (for ([a asserts]) (assert a))))

(define (maximize obj . asserts)
  (optimize #:maximize (if (list? obj) obj (list obj))
            #:guarantee (for ([a asserts]) (assert a))))

(define (check-solution actual test expected)
  (check-sat actual)
  ;(printf "actual: ~a\nexpected: ~a\n" actual expected)
  (define ma (model actual))
  (define me (model expected))
  (check-equal? (dict-keys ma) (dict-keys me))
  (for ([(k v) ma])
    (check test v (dict-ref me k)))
  (define oa (objectives actual))
  (define oe (objectives expected))
  (check-equal? (dict-keys oa) (dict-keys oe))
  (for ([(k v) oa])
    (check-equal? v (dict-ref oe k))))

(define basic-tests
  (test-suite+ "Basic optimize tests with no finitization."
    (current-bitwidth #f)
    (define-symbolic x y z integer?)
    
;    (check-solution
;     (maximize (+ x y) (< x 2) (< (- y x) 1)) =
;     (sat (hash x 1 y 1) (hash (+ x y) 2)))

    (check-solution
     (maximize z (< x 2) (< (- y x) 1) (= z (+ x y))) =
     (sat (hash x 1 y 1 z 2) (hash z (interval 2 #t 2 #t))))
    
    (check-unsat (maximize (+ x y) (< x 2) (< y 2) (> (+ x y) 4)))

    (check-unsat (minimize (+ x y) (< x 2) (< y 2) (> (+ x y) 4)))

;    (check-solution
;     (minimize (+ x y) (< x 4) (< (- y x) 1) (> y 1)) =
;     (sat (hash x 2 y 2) (hash (+ x y) 4)))

;    (check-solution
;     (maximize (+ x y) (< x 2) (> (- y x) 1)) >=
;     (sat (hash x 1 y 3) (hash (+ x y) (interval +inf.0 #t +inf.0 #t))))

;    (check-solution
;     (minimize (+ x y) (< x 4) (< (- y x) 1) (< y 1)) <=
;     (sat (hash x -1 y -1) (hash (+ x y) (interval -inf.0 #t -inf.0 #t))))

    (check-solution
     (maximize (list x y) (< x z) (< y z) (< z 5) (! (= x y))) =
     (sat (hash z 4 y 2 x 3) (hash x (interval 3 #t 3 #t) y (interval 2 #t 2 #t))))

;    (define-symbolic r q real?)
;
;    (check-solution
;     (maximize (+ r q) (< r 4) (< q 5)) >=
;     (sat (hash r 3.0 q 4.0) (hash (+ r q) (interval -inf.0 #f 9 #f))))
;

    ))

(time (run-tests basic-tests))
