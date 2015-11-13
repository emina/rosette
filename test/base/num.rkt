#lang racket

(require rackunit rackunit/text-ui rosette/lib/util/roseunit
         racket/fixnum 
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/num
         rosette/base/core/merge
         (only-in rosette/base/form/define define-symbolic)
         "common.rkt")

(define-symbolic x @number?)
(define-symbolic y @number?)
(define-symbolic z @number?)

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

(define (check-arithmetic op neg agg id)
  (check-nary op id x y z)
  
  ; neg simplifications
  (check-equal? (neg x id) x)
  (check-equal? (neg id x) (neg x))
  (check-equal? (neg x x) id)
  (check-equal? (neg (neg x)) x)
  (check-equal? (neg x y) (op x (neg y)))
  (check-equal? (neg x y z 2) (op x (neg y) (neg z) (neg 2)))
  
  ; op number/expr simplifications
  (check-equal? (op 3 (op 5 x)) (op (op 3 5) x))
  (check-equal? (op (op 4 x) 2) (op (op 2 4) x))
  (check-equal? (op 1 x z 3 y -9) (op x y z (op 1 3 -9)))
  
  ; op/neg simplifications
  (check-equal? (op (neg x) x) id)
  (check-equal? (op x (neg x)) id)
  (check-equal? (op (neg x y) (neg x) y) id)
  (check-equal? (op x (neg z x)) z)
  (check-equal? (op (neg x y) z (neg x) y) z)
  
  (check-equal? (op x (op y (neg x))) y)
  (check-equal? (op x (op (neg x) y)) y)
  (check-equal? (op (op y (neg x)) x) y)
  (check-equal? (op (op (neg x) y) x) y)
  
  (check-equal? (op (neg (op x y)) y) (neg x))
  (check-equal? (op (neg (op x y)) x) (neg y))
  (check-equal? (op y (neg (op x y))) (neg x))
  (check-equal? (op x (neg (op x y))) (neg y))
  
  ; op/agg/neg simplifications
  (check-equal? (op x (agg x 3)) (agg x 4))
  (check-equal? (op (agg x 3) x) (agg x 4))
  (check-equal? (op (neg x) (agg x 3)) (agg x 2))
  (check-equal? (op (agg x 3) (neg x)) (agg x 2))
  (check-equal? (op (agg x 2) (agg x 3)) (agg x 5))
  (check-equal? (op (agg x 3) (agg x 2)) (agg x 5)))

(define (check-expt)
  (check-equal? (@expt x 0) 1)
  (check-equal? (@expt 0 x) 0)
  (check-equal? (@expt x 1) x)
  (check-equal? (@expt (@expt x y) z) (@expt x (@* y z)))
  (check-equal? (@expt 10 2) (expt 10 2)))

(define (check-remainder)
  (check-equal? (@remainder 19 3) (remainder 19 3))
  (check-equal? (@remainder x x) 0)
  (check-equal? (@remainder x 1) 0)
  (check-exn exn:fail? (thunk (@remainder x 0))))

(define (check-eq)
  (check-false (@= 0 1))
  (check-true (@= 1 1))
  (check-true (@= x x))
  (check-false (boolean? (@= x y)))
  (check-equal? (@= y 1) (@= 1 y))
  (check-equal? (@= y x) (@= x y))
  (check-true (@= (@+ x y) (@+ y x)))
  (check-true (@= (@+ x 1) (@+ 1 x))))

(define (check-order)
  (check-false (@< 0 0))
  (check-false (@< x x))
  (check-false (@< 1 0))
  (check-true (@< 0 1))
  (check-true (@<= 0 1))
  (check-true (@<= 0 0))
  (check-true (@<= x x))
  (check-false (@> 0 0))
  (check-false (@> x x))
  (check-true (@> 1 0))
  (check-true (@>= 1 0))
  (check-true (@>= 0 0))
  (check-true (@>= x x))
  
  (check-equal? (@< x y) (@> y x))
  (check-equal? (@< 0 y) (@> y 0))
  (check-equal? (@< x 0) (@> 0 x))
  (check-equal? (@<= x y) (@>= y x))
  (check-equal? (@<= 0 y) (@>= y 0))
  (check-equal? (@<= x 0) (@>= 0 x))
  
  (check-equal? (@< x (@+ x 1)) #t)
  (check-equal? (@<= x (@+ x 1)) #t)
  (check-equal? (@> x (@- x 1)) #t)
  (check-equal? (@>= x (@- x 1)) #t))

(define (check-idempotent-unary-op op racket-op)
  (check-equal? (op -10) (racket-op -10))
  (check-equal? (op 0) (racket-op 0))
  (check-equal? (op 10) (racket-op 10))
  (check-equal? (op (op x)) (op x)))

(define (check-shift-op op racket-op)
  (check-equal? (op 16 2) (racket-op 16 2)  (format "(~a ~a ~a)" op 16 2))
  (check-equal? (op -16 2) (racket-op -16 2) (format "(~a ~a ~a)" op -16 2))
  (check-equal? (op 0 x) 0)  (format "(~a ~a ~a)" op 0 x))

(define (check-bitwise-not)
  (check-equal? (@bitwise-not (@bitwise-not x)) x)
  (check-equal? (@bitwise-not -1) 0)
  (check-equal? (@bitwise-not 0) -1))

(define (check-bitwise op co racket-op id)
  (check-nary op id x y z)
  (check-equal? (op 1 x) (op x 1))
  (check-equal? (op x (@bitwise-not x) ) (@bitwise-not id))
  (check-equal? (op x (@bitwise-not id)) (@bitwise-not id))
  (check-equal? (op (co x y) (@bitwise-not (co x y))) (@bitwise-not id))
  (check-equal? (op (op x y) (@bitwise-not (op x y))) (@bitwise-not id))
  (check-equal? (op x (@bitwise-not id)) (@bitwise-not id))
  (check-equal? (@bitwise-not (@bitwise-not (op x y))) (op x y))
  (check-equal? (op z y x x x x y x z z ) (op x y z))
  (check-equal? (op z y x x x x y x (@bitwise-not z) z ) (@bitwise-not id))
  (check-equal? (op z (co z x)  (co z y) x (co x y)) (op x z))
  (check-equal? (op (co x y) (co x y z)) (co x y))
  (check-equal? (op (co x y z) (co x y)) (co x y))
  (check-equal? (op (co x y) (co x y z) x)  x) 
  (check-equal? (op (co x y z) (co x y) x)  x)
  (check-equal? (op 47361 486193 (op -983823 6481 12 -4240)) 
                (racket-op 47361 486193 -983823 6481 12 -4240)))

(define (check-bitwise-xor)
  (check-equal? (@bitwise-xor) 0)
  (check-equal? (@bitwise-xor 0 0) 0)
  (check-equal? (@bitwise-xor 0 0 0) 0)
  (check-equal? (@bitwise-xor x) x)
  (check-equal? (@bitwise-xor 0 x) x)
  (check-equal? (@bitwise-xor x 0) x)
  (check-equal? (@bitwise-xor x y) (@bitwise-xor y x))
  (check-equal? (@bitwise-xor x x) 0)
  (check-equal? (@bitwise-xor x (@bitwise-not x)) -1)
  (check-equal? (@bitwise-xor (@bitwise-not x) x) -1)
  (check-equal? (@bitwise-xor x -1) (@bitwise-not x))
  (check-equal? (@bitwise-xor -1 x -1) x)
  (check-equal? (@bitwise-xor 47361 486193 (@bitwise-xor -983823 6481 12 -4240)) 
                (bitwise-xor 47361 486193 -983823 6481 12 -4240)))

(define num-tests
  (test-suite+ 
   "Tests for rosette/base/num.rkt" 
   
   (check-arithmetic @+ @- @* 0)
   (check-arithmetic @* @/ @expt 1)
   (check-equal? (@* x 0) 0)
   (check-equal? (@* 0 x) 0)
   (check-equal? (@/ 0 x) 0)
   (check-exn exn:fail? (thunk (@/ x 0)))
   
   (check-expt)
   (check-remainder)
   (check-idempotent-unary-op @abs abs)
   (check-idempotent-unary-op @sgn sgn)
   
   (check-eq)
   (check-order)
   
   (check-bitwise-not)
   (check-bitwise @bitwise-and @bitwise-ior bitwise-and -1)
   (check-bitwise @bitwise-ior @bitwise-and bitwise-ior 0)
   (check-bitwise-xor)
   (check-shift-op @<< fxlshift)
   (check-shift-op @>> fxrshift)
   (check-shift-op @>>> 
                   (lambda (x y) (fxrshift (fxand x (fxnot (fxlshift -1 (current-bitwidth)))) y)))))
  
(time (run-tests num-tests))
