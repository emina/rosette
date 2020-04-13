#lang racket

(require
  (only-in racket/splicing splicing-let)
  "bitvector.rkt" "merge.rkt" "safe.rkt" "term.rkt" "bool.rkt" "forall.rkt" "lift.rkt"
  (only-in "real.rkt" @integer? @> @>= @=)
  (only-in "numerics.rkt" extreme))

(provide bit lsb msb bvzero? bvadd1 bvsub1
         bvsmin bvsmax bvumin bvumax
         rotate-left rotate-right bvrol bvror
         bool->bitvector bitvector->bool bitvector->bits)

(define-syntax (define-lifted stx)
  (syntax-case stx ()
    [(_ (id arg ...) expr ...)
     #'(define-lifted id (lambda (arg ...) expr ...))]
    [(_ id impl)
     #'(define id (procedure-rename (bvlift-op impl) 'id))]))

(define (bit i x)
  (@extract i i x))

(define (lsb x) (bit 0 x))

(define-lifted (msb x)
  (let ([pos (sub1 (bitvector-size (get-type x)))])
    (bit pos x)))

(define-lifted bvsmin (curry extreme @bvsle))
(define-lifted bvsmax (curry extreme @bvsge))
(define-lifted bvumin (curry extreme @bvule))
(define-lifted bvumax (curry extreme @bvuge))

(define (bool->bitvector x [t 1])
  (merge (@false? x) (bv 0 t) (bv 1 t)))

(define (bitvector->bool x)
  (! (bvzero? x)))

(define-lifted (bvzero? x)
  (@bveq x (bv 0 (get-type x))))

(define-lifted (bvadd1 x)
  (@bvadd x (bv 1 (get-type x))))

(define-lifted (bvsub1 x)
  (@bvsub x (bv 1 (get-type x))))

(define-lifted (bitvector->bits v)
  (for/list ([i (bitvector-size (get-type v))])
    (bit i v)))

(define-syntax-rule (define-rotate id proc)
  (splicing-let ([dir proc])
    (define (id @i @x)
      (define i (type-cast @integer? @i 'id))
      (define x (bvcoerce @x id))
      (match i
        [0 x]
        [_
         (assert (@>= i 0) (arguments-error 'id "expected i >= 0" "i" i))
         (for/all ([x x])
           (let ([sz (bitvector-size (get-type x))])
             (assert (@> sz i) (arguments-error 'id "expected (size-of x) > i" "x" x "i" i))
             (if (integer? i)
                 (dir i sz x)
                 (merge+ (cons (cons (@= i 0) x)
                               (for/list ([n (in-range 1 sz)])
                                 (cons (@= n i)
                                       (dir n sz x))))
                         #:unless sz
                         #:error (arguments-error 'id "expected (size-of x) > i >= 0" "x" x "i" i)))))]))))

(define-rotate rotate-left 
   (lambda (i sz x)
     (@concat (@extract (- sz i 1) 0 x) (@extract (- sz 1) (- sz i) x))))

(define-rotate rotate-right
   (lambda (i sz x)
     (@concat (@extract (- i 1) 0 x) (@extract (- sz 1) i x))))        
         
; x and y must be bitvectors (not unions) of the same length.
; shift1 and shift2 are shift operators.
(define-syntax-rule (bvrotate x y shift1 shift2)
  (let* ([sz (bitvector-size (get-type y))]
         [n (bv sz sz)]
         [amount (@bvurem y n)])
    (@bvor (shift1 x amount) (shift2 x (@bvsub n amount)))))

(define-lifted (bvrol x y) (bvrotate x y @bvshl @bvlshr))
(define-lifted (bvror x y) (bvrotate x y @bvlshr @bvshl))
  