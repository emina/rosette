#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-symbolic a b c boolean?)
(define-symbolic xi yi zi integer?)
(define-symbolic xr yr zr real?)
(define-symbolic xb yb zb (bitvector 4))

(define basic-tests
  (test-suite+ "Basic synthesis tests."
    (current-bitwidth #f)
    
    (check-unsat (synthesize #:forall '() #:guarantee (assert #f)))
    (check-unsat (synthesize #:forall '() #:guarantee (begin (assume #f) (assert #f))))

    (check-equal?
     (model (check-sat (synthesize #:forall a #:guarantee (begin (assume a) (assert (&& a b))))))
     (hash b #t))
    
    (check-unsat (synthesize #:forall xb #:guarantee (assert (bvslt xb (bvadd xb yb)))))
    
    (parameterize ([current-bitwidth 4])
      (check-unsat (synthesize #:forall xi #:guarantee (assert (< xi (+ xi yi))))))
    
    (check-true
     (evaluate (> yi 0) (synthesize #:forall xi #:guarantee (assert (< xi (+ xi yi))))))
    
    (check-true
     (evaluate (> yr 0) (synthesize #:forall xr #:guarantee (assert (< xr (+ xr yr))))))
    
    (check-true
     (evaluate (> yr 0) (synthesize #:forall xi #:guarantee (assert (< xi (+ xi yr))))))
    
    (check-true
     (evaluate (>= yi 0) (synthesize #:forall xi #:guarantee (begin  (assume (> xi 0)) (assert (>= xi yi))))))
    
    ))

(module+ test
  (time (run-tests basic-tests)))
