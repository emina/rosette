#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit "../model/reals.rkt")

(define-symbolic a b c boolean?)
(define-symbolic x integer?)
(define-symbolic y real?)
(define x4 (int4 1 2 3 4))
   
(define scalar-tests
  (test-suite+ 
   "Tests for scalar types"
   
   (check-equal? (bool #t) #t)
   (check-equal? (bool #f) #f)
   (check-equal? (bool a) a)
   (check-equal? (real-type-of (bool a)) bool)
   (check-exn exn:fail? (thunk (bool 1)))
   (check-exn exn:fail? (thunk (bool x)))
   
   (check-equal? ((bool) #t) #t)
   (check-equal? ((bool) #f) #f)
   (check-equal? ((bool) 5) #t)
   (check-equal? ((bool) 0) #f)
   (check-equal? ((bool) 0.0) #f)
   (check-equal? ((bool) a) a)
   (check-equal? (real-type-of ((bool) a)) bool)
   (check-equal? ((bool) x) (! (= x 0)))
   
   (check-equal? (int 5) 5)
   (check-equal? (int x) x)
   (check-equal? (real-type-of (int x)) int)
   (check-exn exn:fail? (thunk (int #f)))
   (check-exn exn:fail? (thunk (int a)))
   (check-exn exn:fail? (thunk (int 3.5)))
   
   (check-equal? ((int) #t) 1)
   (check-equal? ((int) #f) 0)
   (check-equal? ((int) 5) 5)
   (check-equal? ((int) 10.98437587245) 10)
   (check-equal? ((int) a) (if a 1 0))
   (check-equal? ((int) x) x)
   (check-equal? (real-type-of ((int) x)) int)
   (check-equal? ((int) (if a x y)) (if a x (real->integer y)))
   (check-equal? ((int) (if a a y)) (if a 1 (real->integer y)))
   (check-equal? ((int) (if a a 3.5)) (if a 1 3))
   
   (check-equal? (float 5.5) 5.5)
   (check-equal? (float y) y)
   (check-exn exn:fail? (thunk (float #f)))
   (check-exn exn:fail? (thunk (float a)))
   (check-exn exn:fail? (thunk (float 3)))
   
   (check-equal? ((float) #t) 1.0)
   (check-equal? ((float) #f) 0.0)
   (check-equal? ((float) 5) 5.0)
   (check-equal? ((float) 10.98437587245) 10.98437587245)
   (check-equal? ((float) a) (if a 1.0 0.0))
   (check-equal? ((float) x) (integer->real x))
   (check-equal? ((float) (if a x y)) (if a (integer->real x) y))
   (check-equal? ((float) (if a a y)) (if a 1.0 y))
   (check-equal? ((float) (if a a 3.5)) (if a 1.0 3.5))))

(define vector-tests
  (test-suite+ 
   "Tests for vector types"

   (check-equal? ((int2) a) (int2 (if a -1 0) (if a -1 0)))
   (check-equal? ((int2) y) (int2 (real->integer y) (real->integer y)))
   (check-equal? ((int2) 3) (int2 3 3))
   (check-equal? ((int2) 3.5) (int2 3 3))
   (check-equal? ((int3) #t) (int3 -1 -1 -1))
   (check-equal? ((int4) #f) (int4 0 0 0 0))
   (check-equal? ((int2) (int2 0 1)) (int2 0 1))
   
   (check-equal? ((int2) (if a x '())) (int2 x x))
   (check-equal? ((int2) (if a x #t)) (if a ((int2) x) ((int2) #t)))
   
   (check-exn exn:fail? (thunk ((int2) (float2 3.5 3.5))))
   (check-exn exn:fail? (thunk (int2 3.5 3.5)))
   (check-exn exn:fail? (thunk (int2 1 #t)))
   (check-exn exn:fail? (thunk (int2 a 1)))
      
   (for ([i (in-range 0 4)])
     (check-equal? (vector-ref x4 i) (add1 i)))
   (check-exn exn:fail? (thunk (vector-set! x4 3 6)))))

(time (run-tests scalar-tests))
(time (run-tests vector-tests))
