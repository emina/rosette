#lang rosette

(require rackunit rackunit/text-ui  
         rosette/lib/roseunit)

(define-symbolic a b c boolean?)
(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)
(define-symbolic xb yb zb (bitvector 4))

(define consts (set a b c xi yi zi xr yr zr xb yb zb))

(define (check-solve+ . bools)
  (define gen (solve+))
  (define-values (head tail) (split-at bools (sub1 (length bools))))
  ; Check that all solutions prior to last are sat.
  (for ([h head])
    (check-sat (gen h)))
  ; Check that the last solution is unsat
  (check-unsat (gen (car tail)))
  ; Check that next-to-last solution is still sat
  (check-sat (gen 1))
  ; Check that nothing can be called after shutdown
  (gen 'shutdown)
  (check-exn exn:fail? (thunk (gen #t)))
  (check-exn exn:fail? (thunk (gen 1)))
  (check-exn exn:fail? (thunk (gen 'shutdown)))
  ; Check that term cache is not polluted with finitizaton values -- no
  ; fresh constants should be left in the term cache
  (check subset? (apply set (symbolics (terms))) consts))

(define basic-tests
  (test-suite+ "Solve+ tests with no (effective) finitization."
    (current-bitwidth #f)

    (check-solve+ #t #f)
    (check-solve+ #f)
    (check-solve+ (> xi (+ xi yr)) (= yr 2.5))
    (check-solve+ (< xi (+ xi yr)) (= yr 2.5) (> 0 yr))
    (check-solve+ (> xi (+ xi (bitvector->integer yb))) (bveq yb (bv 1 4)))    
    (check-solve+ (< xi (+ xi (bitvector->integer yb))) (bveq yb (bv 1 4)) (= xi 0) (< xi 0))
    (check-solve+ (! (= yr 0)) (= (/ xr yr) zr) (= zr 1.5) (! (= xr (* yr 1.5)))) 
    (check-solve+ (= (* xr yr) zr) (= zr 20000) (= zr 0))
    
    (current-bitwidth 4)
    (check-solve+ #t #f)
    (check-solve+ #f)
    (check-solve+ (bveq xb yb) (bveq xb (bv 3 4)) (! (bveq yb (bv 3 4))))
    (check-solve+ (bveq xb yb) (bveq xb (bv 0 4)) (bveq yb (bv 1 4)))
    ))

(define finitized-tests
  (test-suite+ "Solve+ tests with finitization."
    (current-bitwidth 5)
    
    (check-solve+ (! (= yr 0)) (= (/ xr yr) zr) (= zr 2) (= xr 5) (! (= yr 2)))
    (check-solve+ (= xr (bitvector->integer yb)) (bveq yb (bv 2 4)) (! (= xr 2)))
    
    (check-solve+ (! (= yr 0)) (= (/ xr yr) zr) (= zr 1.5) (! (= zr 1)))
    (check-solve+ (= (* xr yr) zr) (= zr 20000) (! (= zr 0)))
    
    (current-bitwidth 3)
    (check-solve+ (= xr (bitvector->integer yb)) (bveq yb (bv -8 4)) (! (= xr 0)))
 )) 

(module+ test             
  (time (run-tests basic-tests))
  (time (run-tests finitized-tests)))
