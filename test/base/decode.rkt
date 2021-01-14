#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/solver/smt/dec rosette/base/core/polymorphic)

; Solution procedure by Z3 containing 'let' expressions.
(define sol
  '#hash((c0 . (define-fun c0 () (_ BitVec 64) |#x0000000000000000|))
         (c16 . (define-fun c16 ((x!0 (_ BitVec 64))) (_ BitVec 64)
                  (let ((a!1 (ite (bvule |#x0000000000000011| x!0)
                                  (ite (bvule |#x0000008000000000| x!0)
                                       (ite (= x!0 |#xffffffffffffffff|) |#xffffffffffffffff| |#x0000008000000000|)
                                       |#x0000000000000011|)
                                  |#x0000000000000001|)))
                    (ite (= a!1 |#x0000008000000000|) |#x0000000000000000| |#x0000000000000005|))))
         (c29 . (define-fun c29 ((x!0 (_ BitVec 64))) (_ BitVec 64) |#x0000000000000002|))
         (c4 . (define-fun c4 ((x!0 (_ BitVec 64))) (_ BitVec 64) |#x0000000000000001|))
         (c42 . (define-fun c42 ((x!0 (_ BitVec 64))) (_ BitVec 64)
                  (ite (= (k!11 x!0) |#x0000000000000001|)
                       |#x0000000000000080|
                       (ite (= (k!11 x!0) |#x0000002000000000|) |#x8007feefdf009ff9| |#x0000000000000003|))))
         (c46 . (define-fun c46 ((x!0 (_ BitVec 64))) (_ BitVec 64)
                  (ite (= (k!11 x!0) |#x0000000000000001|) |#x0000000000000001|
                       (ite (= (k!11 x!0) |#x0000002000000000|) |#xad61f9a0021a7fde| |#x0000000000000004|))))
         (c5 . (define-fun c5 () (_ BitVec 64) |#x0000002000000000|))
         (c67 . (define-fun c67 () (_ BitVec 64) |#x52ab07efdfef0000|))
         (c78 . (define-fun c78 () (_ BitVec 64) |#x0000008000000000|))
         (k!11 . (define-fun k!11 ((x!0 (_ BitVec 64))) (_ BitVec 64)
                   (ite (bvule |#x0000000000000011| x!0)
                        (ite (bvule |#x0000002000000000| x!0)
                             (ite (= x!0 |#xffffffffffffffff|) |#xffffffffffffffff| |#x0000002000000000|)
                             |#x0000000000000011|)
                        |#x0000000000000001|)))))

; Constructs an environment, as expected by decode-model, for sol.
(define env
  (let ([constant-id? (lambda (id) (regexp-match? #px"c\\d+" (symbol->string id)))])
    (for/hash ([(k v) sol] #:when (constant-id? k))
      (values
       (match v
         [`(define-fun ,(== k) () (_ BitVec 64) ,_ ...)
          (constant k (bitvector 64))]
         [`(define-fun ,(== k) (,arg ...) (_ BitVec 64) ,_ ...)
          (constant k (apply ~> (make-list (add1 (length arg)) (bitvector 64))))]
         [_ k])
       k))))

; Checks that the two concrete values, v0 and v1, are equivalent.
(define (check-equivalent c v0 v1)
  (check-true
   (unsat?
    (if (procedure? c)
        (local [(define-symbolic* x (bitvector 64) #:length (procedure-arity c))]
          (verify (assert (equal? (apply v0 x) (apply v1 x)))))
        (verify (assert (equal? v0 v1)))))
   (format "solutions for ~a not equivalent" c)))

; Maps the values of env to its key (ok, because env map is 1-1).
(define ~env (for/hash ([(k v) env]) (values v k)))

; Gets the Rosette solution for all constants in env except c16,
; which has the let-expression in its SMT encoding.
(define out0 (decode-model env (hash-remove sol 'c16)))

; Gets the Rosette solution for all constants in env.
(define out1 (decode-model env sol))

; Manual decoding of the solution for c16.
(define (c16λ x!0)
  (define |#x0000000000000011| (bv #x0000000000000011 (bitvector 64)))
  (define |#x0000008000000000| (bv #x0000008000000000 (bitvector 64)))
  (define |#xffffffffffffffff| (bv #xffffffffffffffff (bitvector 64)))
  (define |#x0000000000000001| (bv #x0000000000000001 (bitvector 64)))
  (define |#x0000000000000000| (bv #x0000000000000000 (bitvector 64)))
  (define |#x0000000000000005| (bv #x0000000000000005 (bitvector 64)))
  (define = equal?)
  (let ((a!1 (ite (bvule |#x0000000000000011| x!0)
                  (ite (bvule |#x0000008000000000| x!0)
                       (ite (= x!0 |#xffffffffffffffff|) |#xffffffffffffffff| |#x0000008000000000|)
                       |#x0000000000000011|)
                  |#x0000000000000001|)))
    (ite (= a!1 |#x0000008000000000|) |#x0000000000000000| |#x0000000000000005|)))

(define decode-tests
  (test-suite+
   "SMT decoder tests"
   ; Check that out0 and out1 agree on the value of all constants
   ; they have in common (i.e., everything except c16)
   (for ([(k v) sol] #:when (hash-has-key? ~env k))
     (define c (hash-ref ~env k))
     (define v0 (hash-ref out0 c #f))
     (define v1 (hash-ref out1 c #f))
     (and v0 v1 (check-equivalent c v0 v1)))
   ; Check that the decoded value of c16 is equivalent to the manually extracted one.
   (define c16 (hash-ref ~env 'c16))
   (check-equivalent c16 (hash-ref out1 c16) c16λ)))

(module+ test
  (time (run-tests decode-tests)))

