#lang rosette

(require rackunit rackunit/text-ui (rename-in rackunit [check-exn rackunit/check-exn])
         rosette/lib/roseunit (only-in rosette/base/core/merge merge*))
(require (only-in rosette/base/core/bool vc-assumes vc-asserts) )

(define-symbolic z (bitvector 1))
(define-symbolic x u (bitvector 4))
(define-symbolic y w (bitvector 8))
(define-symbolic i integer?)
(define-symbolic b c boolean?)

(define-syntax-rule (check-exn e ...)
  (begin
    (rackunit/check-exn e ...)
    (clear-vc!)))

(define-syntax-rule (check≡ actual expected)
  (let ([ra (with-vc actual)]
        [re (with-vc expected)])
    (check-pred unsat?
                (verify (assert (equal? (vc-assumes (result-state ra))
                                        (vc-assumes (result-state re))))))
    (check-pred unsat?
                (verify (assert (equal? (vc-asserts (result-state ra))
                                        (vc-asserts (result-state re))))))
    (check-pred unsat?
                (verify (begin (assume (&& (vc-assumes (result-state ra)) (vc-asserts (result-state ra))))
                               (assert (equal? (result-value ra) (result-value re))))))))


(define-syntax check-bv-exn
  (syntax-rules ()
    [(_ expr) (check-bv-exn #px"expected bitvectors of same length" expr)]
    [(_ rx expr)
     (match (with-vc expr)
       [(failed e _)
        (check-pred exn:fail? e)
        (check-true (regexp-match? rx (exn-message e)))]
       [r (check-pred failed? r)])]))

(define (check-unary op1 op c)
  (check≡ (op1 x) (op x (bv c 4)))
  (check≡ (op1 y) (op y (bv c 8)))
  (check≡ (op1 (if b x y)) (op (if b x y) (if b (bv c 4) (bv c 8))))
  (check≡ (op1 (if b x 'foo)) (op (if b x 'foo) (if b (bv c 4) (bv c 8))))
  (check≡ (op1 (if b x (if c y 'foo))) (op (if b x (if c y 'foo)) (if b (bv c 4) (if c (bv c 8) 'foo))))
  (check-exn #px"expected bitvectors" (thunk (op1 (if b 'bar 'foo))))
  (check-exn #px"expected bitvectors" (thunk (op1 1))))

(define (check-bit)
  (check≡ (bit i x) (extract i i x))
  (check≡ (bit i y) (extract i i y))
  (check≡ (bit i (if b x y)) (extract i i (if b x y)))
  (check≡ (bit (if b i 'foo) x) (begin (assert b) (extract i i x)))
  (check≡ (bit (if b i 'foo) (if c x 'bar)) (begin (assert b) (assert c) (extract i i x)))
  (check≡ (bit (if b 3 7) x) (begin (assert b) (extract 3 3 x)))
  (check≡ (bit (if b 3 7) y) (if b (extract 3 3 y) (extract 7 7 y)))
  (for ([i 4][j '(0 1 0 1)])
    (check-equal? (bit i (bv #b1010 4)) (bv j 1)))
  (check-exn #px">= 0" (thunk (bit -1 x)))
  (check-exn #px">" (thunk (bit 4 x))))

(define (check-lsb)
  (check≡ (lsb x) (extract 0 0 x))
  (check≡ (lsb (if b x y)) (extract 0 0 (if b x y)))
  (check≡ (lsb (bv #b01 2)) (bv 1 1))
  (check≡ (lsb (bv #b00 2)) (bv 0 1))
  (check≡ (lsb (if b x (bv #b01 2))) (if b (extract 0 0 x) (bv 1 1)))
  (check≡ (lsb (if b 'foo (bv #b01 2))) (begin (assert (! b)) (bv 1 1)))
  (check-exn #px"expected" (thunk (lsb -1))))

(define (check-msb)
  (check≡ (msb x) (extract 3 3 x))
  (check≡ (msb y) (extract 7 7 y))
  (check≡ (msb (if b x y)) (extract (if b 3 7) (if b 3 7) (if b x y)))
  (check≡ (msb (bv #b10 2)) (bv 1 1))
  (check≡ (msb (bv #b00 2)) (bv 0 1))
  (check≡ (msb (if b x (bv #b01 2))) (if b (extract 3 3 x) (bv 0 1)))
  (check≡ (msb (if b 'foo (bv #b01 2))) (begin (assert (! b)) (bv 0 1)))
  (check-exn #px"expected" (thunk (msb -1))))

(define (check-bv<->bool)
  (check≡ (bool->bitvector #t) (bv 1 1))
  (check≡ (bool->bitvector #f) (bv 0 1))
  (check≡ (bool->bitvector 'foo) (bv 1 1))
  (check≡ (bool->bitvector b) (if b (bv 1 1) (bv 0 1)))
  (check≡ (bool->bitvector (if b 1 'foo)) (bv 1 1))
  (check≡ (bool->bitvector #t 4) (bv 1 4))
  (check≡ (bool->bitvector #f 4) (bv 0 4))
  (check≡ (bool->bitvector 'foo 4) (bv 1 4))
  (check≡ (bool->bitvector b 4) (if b (bv 1 4) (bv 0 4)))
  (check≡ (bool->bitvector (if b 1 'foo) 4) (bv 1 4))
  (check≡ (bitvector->bool x) (! (bvzero? x)))
  (check≡ (bitvector->bool y) (! (bvzero? y)))
  (check≡ (bitvector->bool (bv 0 4)) #f)
  (check≡ (bitvector->bool (bv 2 4)) #t)
  (check≡ (bitvector->bool (if b (bv 0 4) (bv 1 8))) (! b))
  (check≡ (bitvector->bool (if b (bv 0 4) 'foo)) (begin (assert b) #f))
  (check≡ (bitvector->bool (if b x 'foo)) (begin (assert b) (! (bvzero? x))))
  (check≡ (bool->bitvector (bitvector->bool (bv 0 1))) (bv 0 1))
  (check≡ (bool->bitvector (bitvector->bool (bv 1 1))) (bv 1 1))
  (check≡ (bool->bitvector (bitvector->bool z)) z)
  (check≡ (bitvector->bool (bool->bitvector b)) b)
  (check-exn #px"expected" (thunk (bitvector->bool  -1))))

(define (check-bitvector->bits)
  (define xbits (for/list ([i 4]) (bit i x)))
  (define ybits (for/list ([i 8]) (bit i y)))
  (check≡ (bitvector->bits x) xbits)
  (check≡ (bitvector->bits y) ybits)
  (check≡ (bitvector->bits z) (list z))
  (check≡ (bitvector->bits (if b z x)) (if b (list z) xbits))
  (check≡ (bitvector->bits (if b z (if c x y))) (if b (list z) (if c xbits ybits)))
  (check≡ (bitvector->bits (if b z (if c x 'foo))) (begin (assert (|| b (&& c (! b)))) (if b (list z) xbits)))
  (check≡ (bitvector->bits (if b x 'foo)) (begin (assert b) xbits))
  (check≡ (bitvector->bits (bv #b1010 4)) (for/list ([i '(0 1 0 1)]) (bv i 1)))
  (check≡ (apply concat (reverse (bitvector->bits x))) x)
  (check-exn #px"expected" (thunk (bitvector->bits (if b -1 'foo)))))

(define (check-min/max op cmp limit)
  (check≡ (op x) x)
  (check≡ (op x (limit 4)) (limit 4))
  (check≡ (op x u) (if (cmp x u) x u))
  (check≡ (op x u (limit 4)) (limit 4))
  (check≡ (op x (if b u y) (limit 4)) (begin (assert b) (limit 4)))
  (check≡ (op (if c x w) (if b u y) (limit 4)) (begin (assert b) (assert c) (limit 4)))
  (check≡ (op (if c x w) (if b u y))
          (begin
            (assert (|| (&& b c) (&& (! b) (! c))))
            (merge* (cons (&& b c) (op x u))
                    (cons (&& (! b) (! c)) (op w y)))))
  (check-exn #px"expected" (thunk (op x y))))

(define (check-bvrotate op opp)
  (check≡ (op x (bv 0 4)) x)
  (check≡ (op y (bv 0 8)) y)
  (check≡ (op (if b x 1) (bv 0 4)) (begin (assert b) x))
  (check≡ (op (if b x y) (if c (bv 0 4) (bv 0 8)))
          (begin (assert (|| (&& b c) (&& (! b) (! c))))
                 (merge* (cons (&& b c) x)
                         (cons (&& (! b) (! c)) y))))
  (check≡ (op (if b x y) (if c u w))
          (begin (assert (|| (&& b c) (&& (! b) (! c))))
                 (merge* (cons (&& b c) (op x u))
                         (cons (&& (! b) (! c)) (op y w)))))
  (check≡ (op x (bv 8 4)) x)
  (check≡ (op x (bv 5 4)) (op x (bv 1 4)))
  (check≡ (op x (bv -1 4)) (op x (bv 3 4)))
  (check≡ (opp (op x u) u) x)
  (check≡ (opp (op (if b x y) (if c u w)) (if c u w))
          (begin (assert (|| (&& b c) (&& (! b) (! c))))
                 (merge* (cons (&& b c) x)
                         (cons (&& (! b) (! c)) y)))))

(define (check-rotate op opp bvop)
  (check≡ (op 0 x) x)
  (check≡ (op 0 y) y)
  (check≡ (op 0 (if b x 1)) (begin (assert b) x))
  (check≡ (op (if c 0 'foo) (if b x y)) (begin (assert c) (if b x y)))
  (check≡ (op i x)
          (begin
            (assert (<= 0 i))
            (assert (< i 4))
            (bvop x (integer->bitvector i (bitvector 4)))))
  (check≡ (op i (if b x y))
          (begin
            (assert (<= 0 i))
            (assert (=> b (< i 4)))
            (assert (=> (! b) (< i 8)))
            (if b
                (bvop x (integer->bitvector i (bitvector 4)))
                (bvop y (integer->bitvector i (bitvector 8))))))
  (check≡ (opp i (op i x ))
          (begin
            (assert (<= 0 i))
            (assert (< i 4))
            x))
  (check≡ (opp i (op i (if b x y )))
          (begin
            (assert (<= 0 i))
            (assert (=> b (< i 4)))
            (assert (=> (! b) (< i 8)))
            (if b x y)))
  (check-exn #px">" (thunk (op 5 x)))
  (check-exn #px">=" (thunk (op -1 x))))

(define tests:bvadd1
  (test-suite+
   "Tests for bvadd1 in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-unary bvadd1 bvadd 1)))

(define tests:bvsub1
  (test-suite+
   "Tests for bvsub1 in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-unary bvsub1 bvsub 1)))

(define tests:bvzero?
  (test-suite+
   "Tests for bvzero? in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-unary bvzero? bveq 0)))

(define tests:bit
  (test-suite+
   "Tests for bit in rosette/base/bvlib.rkt"
   #:features '(qf_bv qf_lia)
   (check-bit)))

(define tests:lsb
  (test-suite+
   "Tests for lsb in rosette/base/bvlib.rkt"
   #:features '(qf_bv qf_lia)
   (check-lsb)))

(define tests:msb
  (test-suite+
   "Tests for msb in rosette/base/bvlib.rkt"
   #:features '(qf_bv qf_lia)
   (check-msb)))

(define tests:bv<->bool
  (test-suite+
   "Tests for bool->bitvector and bitvector->bool in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-bv<->bool)))

(define tests:bv->bits
  (test-suite+
   "Tests for bitvector->bits in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-bitvector->bits)))

(define tests:bvumin
  (test-suite+
   "Tests for bvumin in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-min/max bvumin bvule (lambda (sz) (bv 0 sz)))))

(define tests:bvsmin
  (test-suite+
   "Tests for bvsmin in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-min/max bvsmin bvsle (lambda (sz) (bv (- (expt 2 (sub1 sz))) sz)))))

(define tests:bvumax
  (test-suite+
   "Tests for bvumax in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-min/max bvumax bvuge (lambda (sz) (bv -1 sz)))))

(define tests:bvsmax
  (test-suite+
   "Tests for bvsmax in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-min/max bvsmax bvsge (lambda (sz) (bv (sub1 (expt 2 (sub1 sz))) sz)))))

(define tests:bvrol
  (test-suite+
   "Tests for bvrol in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-bvrotate bvrol bvror)))

(define tests:bvror
  (test-suite+
   "Tests for bvror in rosette/base/bvlib.rkt"
   #:features '(qf_bv)
   (check-bvrotate bvror bvrol)))

(define tests:rotate-left
  (test-suite+
   "Tests for rotate-left in rosette/base/bvlib.rkt"
   #:features '(qf_lia qf_bv)
   (check-rotate rotate-left rotate-right bvrol)))

(define tests:rotate-right
  (test-suite+
   "Tests for rotate-right in rosette/base/bvlib.rkt"
   #:features '(qf_lia qf_bv)
   (check-rotate rotate-right rotate-left bvror)))

(module+ test
  (time (run-tests tests:bvadd1))
  (time (run-tests tests:bvsub1))
  (time (run-tests tests:bvzero?))
  (time (run-tests tests:bit))
  (time (run-tests tests:lsb))
  (time (run-tests tests:msb))
  (time (run-tests tests:bv<->bool))
  (time (run-tests tests:bv->bits))
  (time (run-tests tests:bvumin))
  (time (run-tests tests:bvsmin))
  (time (run-tests tests:bvumax))
  (time (run-tests tests:bvsmax))
  (time (run-tests tests:bvrol))
  (time (run-tests tests:bvror)) 
  (time (run-tests tests:rotate-left))
  (time (run-tests tests:rotate-right))
  )
