#lang racket

(require rackunit rackunit/text-ui
         rosette/query/eval rosette/base/define
         rosette/solver/solution
         rosette/base/term rosette/base/bool rosette/base/num 
         rosette/base/merge rosette/base/procedure
         rosette/base/enum rosette/base/equality)

(provide run-tests-with)

(define solver #f)

(define-symbolic x @number?)
(define-symbolic y @number?)
(define-symbolic z @number?)

(define-symbolic v @number?)
(define-symbolic u @number?)

(define-symbolic a @boolean?)
(define-symbolic b @boolean?)
(define-symbolic c @boolean?)

(define-enum idx '(a b c d e f g h))
(define-symbolic e idx? [8])
    
(define bits (current-bitwidth))
(define min-bv (- (arithmetic-shift 1 (- bits 1))))
(define max-bv (- (arithmetic-shift 1 (- bits 1)) 1))

(define =/bv
  (let ([mask (+ max-bv 1)])
    (lambda (a b) (= (modulo a mask) (modulo b mask)))))

(define (check-sat model-validator . formulas)
  (send solver clear)
  (for ([f formulas])
    (send solver assert f))
  (let ([sol (send solver solve)])
    ;(printf "~s\n" formulas)
    (check-true (sat? sol) (format "no solution found for ~s" formulas))
    (model-validator formulas sol)))

(define (unique-solution-validator model)
  (lambda (formulas sol)
    (for ([(var expected-value) (in-hash model)])
      (let ([actual-value (sol var)])
        (check-equal? actual-value expected-value  
                    (format "expected (equal? ~s ~s) but the solution is ~s=~s for ~s" 
                            var expected-value var actual-value formulas))))))

(define (default-validator formulas sol)
  (for ([f formulas])
    (check-equal? (evaluate f sol) #t 
                  (format "solution violates ~s: ~s" f sol)))) 

(define (div-validator formulas sol)
  (for ([f formulas])
    (match f
      [(expression (== @=) (? number? n) other)
       (check-equal? (truncate (evaluate other sol)) n
                     (format "solution violates ~s: ~s" f sol))]))) 

(define-syntax-rule (test-suite-for name expr ...)
  (test-suite
   (format "~a: ~a" (string-titlecase name) solver)
   #:before (thunk (printf "Testing ~a with ~a\n" name solver))
   expr ...))

(define bool-tests
  (test-suite-for "all boolean operators"
   
   (for ([op (list && || => <=>)])
     (check-sat default-validator (op a b))
     (check-sat default-validator (op (! a) b))
     (check-sat default-validator (op a (! b)))
     (check-sat default-validator (op (! a) (! b)))
     (check-sat default-validator (! (op a b))))
   
   (for ([op (list && ||)] [co (list || &&)])
     (check-sat default-validator (op a b c))
     (check-sat default-validator (op a b (! c)))
     (check-sat default-validator (<=> (op a b c) (op a (op b c))))
     (check-sat default-validator (<=> (op a b c) (op (op a b) c)))
     (check-sat default-validator (<=> (op a b c) (op (op a c) b )))
     (check-sat default-validator (<=> (op a (co b c)) (co (op a b) (op a c))))
     (check-sat default-validator (<=> (! (op a b c)) (co (! a) (! b) (! c)))))
   
   (check-sat default-validator (@false? a))
   (check-sat default-validator (! (@false? a)))))

(define comparison-tests
  (test-suite-for "all bitvector comparison operators" 
  
   ; check equalities
   (for ([i (in-range min-bv (+ 1 max-bv))])
     (check-sat default-validator (@= x i))
     (check-sat default-validator (@= x i) (@= y x))
     (check-sat default-validator (@= x i) (@= y x) (@= y z)))
   
   ; check comparisons. note that some of these 
   ; yield counterinitutive results due to finite arithmetic:
   ; e.g., with a bitwidth of 5, -16 > 14. so the tests are written avoid 
   ; such corner cases for now.
   (for ([i (in-range min-bv max-bv)])
     (check-sat default-validator (@> x i) (@>= y x))
     (check-sat default-validator (@>= x i) (@> y x) (@< z y))
     (check-sat default-validator (@<= x i) (@= x i))
     (check-sat default-validator (@< i x) (@= y x))
     (check-sat default-validator (@= x i) (@= x (@- y 1)) (@= y (+ i 1))))
   ))

(define unary-and-bitwise-tests
  (test-suite-for "all unary and bitwise bitvector operators"
   
   (for ([i (in-range (add1 min-bv) (add1 max-bv))])
     (check-sat default-validator (@= x i) (@= (@abs x) (abs i))))
   
   (for ([i (in-range min-bv (add1 max-bv))])
     (check-sat default-validator 
                (@= x i) 
                (@= (@sgn x) (sgn i))
                (@= (@bitwise-not x) (bitwise-not i))))
   
   (for* ([i (in-range min-bv (add1 max-bv))] [j (in-range min-bv (add1 max-bv))])
     (check-sat default-validator 
                (@= x i) (@= y j) 
                (@= (@bitwise-and x y) (bitwise-and i j))
                (@= (@bitwise-ior x y) (bitwise-ior i j))
                (@= (@bitwise-xor x y) (bitwise-xor i j))
                (@= (@bitwise-not x) (bitwise-not i))))
   
   (check-sat (unique-solution-validator (hash x 3 y 3 z 1))
              (@= x 3) (@= z 1)
              (@= 14 (@>>> (@>> (@<< x y) z) 1)))
))

(define remainder-tests
  (test-suite-for "bitvector remainder"
      
   (for* ([i (in-range min-bv max-bv)] [j (in-range i (add1 max-bv))] #:when (not (= i 0)))
     (check-sat default-validator 
                (@= x i) (@= y j) 
                (@= (@remainder y x) (remainder j i))))))

(define div-tests
  (test-suite-for "bitvector division"
   
   (for* ([i (in-range min-bv max-bv)] [j (in-range i (add1 max-bv))] #:when (not (= i 0)))
     (check-sat div-validator 
                (@= x i) (@= y j) 
                (@= (@/ y x) (truncate (/ j i)))))))

(define (basic-operator-test term-op racket-op)
  (for* ([i (in-range min-bv (add1 max-bv))] [j (in-range min-bv (add1 max-bv))] 
                                             #:when (and (>= (racket-op i j) -16) (< (racket-op i j) 16)))
    (check-sat default-validator 
               (@= x i) (@= y j)
               (@= (term-op x y) (racket-op i j)))))

(define arithmetic-tests
  (test-suite-for "bitvector addition, subtraction and multiplication"  
   (basic-operator-test @+ +)
   (basic-operator-test @- -)
   (basic-operator-test @* *)))

(define precision-tests
  (test-suite-for "precision of bitvector operators"
   
   ; spot check that precision w.r.t. integer division is maintained when possible 
   (check-sat (unique-solution-validator (hash x 4)) (@= 2 (@* (@/ 1 2) x)))
   (check-sat (unique-solution-validator (hash x 4)) (@= 2 (@* 0.5 x))) 
   (check-sat (unique-solution-validator (hash x 15)) (@= 3 (@* (@/ 1 5) x)))
   
   ; spot check exponentiation
   (check-sat (unique-solution-validator (hash x 2)) (@= 4 (@expt x 2)) (@< 0 x) (@< x 4))
   (check-sat (unique-solution-validator (hash x 3)) (@= 9 (@expt x 2)) (@< x 9) (@> x 0))
   (check-sat (unique-solution-validator (hash x 2)) (@= 8 (@expt x 3)) (@< x 8) (@> x 0))
   ))

(define enum-tests
  (test-suite-for "enum comparisons"
    (check-sat default-validator (@eq? (list-ref e 0) (list-ref e 1)))
    (apply check-sat 
           (unique-solution-validator (for/hash ([v e][m (enum-members idx?)]) (values v m)))
           (for/list ([p e] [n (cdr e)])
             (idx<? p n)))))

(define merge-tests
  (test-suite-for "merge"
  
   ; some merge tests
   (check-sat (unique-solution-validator (hash b #t u 1)) 
              (@number? (merge b u c)) 
              (@= (@+ u 2) 3))
   
   (check-sat (unique-solution-validator (hash b #f u -1 v -1)) 
              (@number? (merge b c u)) 
              (@= 2 (@+ u 3)) 
              (@= u v))))

(define type-tests
  (test-suite-for "types"
   
   (check-sat (unique-solution-validator (hash x 6 u 5))  
              (@number? u)
              (@= x (@+ u 1))
              (@< u 6)
              (@> u 4))
   
   (check-sat (unique-solution-validator (hash x 3 y 5 a #t b #t)) 
              (@boolean? a)
              (@boolean? b)
              (let-values ([(g v) (cast @number? (merge (&& a b) 3 y))])
                (&& g (@= x v)))
              (@< x 5)
              (@< y 6)
              (@> y 4))))

(define (run-tests-with s)
  (set! solver s)
  (time (run-tests bool-tests))
  (time (run-tests comparison-tests))
  (time (run-tests unary-and-bitwise-tests))
  (time (run-tests arithmetic-tests))
  (time (run-tests div-tests))
  (time (run-tests remainder-tests))
  (time (run-tests precision-tests))
  (time (run-tests enum-tests))
  (time (run-tests merge-tests))
  (time (run-tests type-tests))
  (send solver shutdown))

;(require rosette/solver/kodkod/kodkod)
;(set! solver (new kodkod%))
;(check-sat div-validator (@= x -16) (@= y -16) (@= (@/ y x) 1))