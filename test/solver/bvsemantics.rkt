#lang racket

(require rackunit rackunit/text-ui
         rosette/query/eval rosette/base/define
         rosette/config/config rosette/solver/solution
         rosette/base/term rosette/base/bool rosette/base/num rosette/base/merge
         rosette/base/equality 
         rosette/solver/kodkod/kodkod rosette/solver/z3/z3)

(define kodkod (new kodkod%))
(define z3 (new z3%))

(define bits 3)
(define min-bv (- (arithmetic-shift 1 (- bits 1))))
(define max-bv (arithmetic-shift 1 (- bits 1)))

(define-syntax-rule (test-suite-for name expr ...)
  (test-suite
   (format "~a: ~a and ~a" (string-titlecase name) kodkod z3)
   #:before (thunk (printf "Testing ~a with ~a and ~a\n" name kodkod z3))
   expr ...))

(define-symbolic x @number?)
(define-symbolic y @number?)
(define-symbolic z @number?)

(define (to-num val)
  (if (equal? @boolean? (type-of val)) (merge val 1 0) val))

(define (solve solver op x-val [y-val #f])
  (with-configuration ([bitwidth bits])
    (send solver clear)
    (send solver assert (@= x x-val))
    (cond [y-val (send solver assert (@= y y-val))
                 (send solver assert (@= z (to-num (op x y))))]
          [else  (send solver assert (@= z (to-num (op x))))])
    (define sol (send solver solve))
    (check-true (sat? sol) (format "no solution found for (~a ~a ~a)" op x-val y-val))
    (solution->list sol)))

(define (check-agree? op x-val [y-val #f])
  (define ksol (solve kodkod op x-val y-val))
  (define zsol (solve z3 op x-val y-val))
  (check-equal? ksol zsol (format "(~a ~a ~a): z3 = ~a, kodkod = ~a" op x-val y-val zsol ksol)))


(define comparison-tests
  (test-suite-for "all bitvector comparison operators" 
  
   (for* ([i (in-range min-bv max-bv)] [j (in-range min-bv max-bv)])
     (check-agree? @> i j)
     (check-agree? @< i j)
     (check-agree? @>= i j)
     (check-agree? @<= i j)
     (check-agree? @= i j)
   )))

(define unary-and-bitwise-tests
  (test-suite-for "all unary and bitwise bitvector operators"
                  
   (for* ([i (in-range min-bv max-bv)] [j (in-range min-bv max-bv)])
     (check-agree? @bitwise-not i)
     (check-agree? @bitwise-and i j)
     (check-agree? @bitwise-ior i j)
     (check-agree? @bitwise-xor i j)
     (check-agree? @<< i j)
     (check-agree? @>> i j)
     (check-agree? @>>> i j))))

(define arithmetic-tests
  (test-suite-for "bitvector addition, subtraction, multiplication, division and remainder"  
   (for* ([i (in-range min-bv max-bv)] [j (in-range min-bv max-bv)])
     (check-agree? @- i)
     (check-agree? @- i j)
     (check-agree? @+ i j)
     (unless (= j 0)
       (check-agree? @/ i j)
       (check-agree? @remainder i j)))))

(time (run-tests comparison-tests))
(time (run-tests unary-and-bitwise-tests))
(time (run-tests arithmetic-tests))
