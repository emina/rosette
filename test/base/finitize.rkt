#lang racket

(require rackunit rackunit/text-ui 
         rosette/solver/smt/z3  rosette/solver/solution 
         rosette/lib/util/roseunit 
         rosette/base/core/term rosette/base/core/bool
         rosette/base/core/real (except-in rosette/base/core/bitvector bv)
         rosette/base/core/finitize
         rosette/base/core/polymorphic rosette/base/core/merge 
         rosette/base/core/assert
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/equality @equal?)
         (only-in rosette/base/core/bitvector [bv @bv])
         (only-in rosette evaluate))

(define solver (new z3%))
(current-bitwidth #f)

(define bw 4)

(define BV (bitvector bw))
(define (bv v [t BV]) (@bv v t))

(define-symbolic a b c d e f g @boolean?)
(define-symbolic xi yi zi @integer?)
(define-symbolic xr yr zr @real?)
(define-symbolic xb yb zb BV)

(define (terms t) ; produces a hashmap from each typed? subterm in t to itself
  (define env (make-hash))
  (define (rec v)
    (when (typed? v)
      (unless (hash-has-key? env v)
        (hash-set! env v v)
        (match v 
          [(expression _ x ...) (for-each rec x)]
          [_ (void)]))))
  (rec t)
  (for/hash ([(k v) env]) (values k v)))

(define (check-pure-bitvector-term t)
  (define expected (terms t))
  (define actual (for/hash ([(k v) (finitize (list t) bw)] #:when (typed? k))
                   (values k v)))
  ;(printf "expected: ~a\nactual: ~a\n" expected actual)
  (check-equal? actual expected))
  
     
(define tests:pure-bitvector-terms
  (test-suite+
   "Tests for finitization of pure BV terms."
   (check-pure-bitvector-term (bv 0))
   (check-pure-bitvector-term xb)
   (check-pure-bitvector-term (@bvadd xb yb (bv 3)))
   (check-pure-bitvector-term (@bvadd xb (@bvmul yb zb) (bv 3)))
   (check-pure-bitvector-term (@bvslt xb (@bvsdiv (bv 3) zb)))
   (check-pure-bitvector-term (@concat xb (@bvand yb zb) (bv 11)))
   (check-pure-bitvector-term (@extract 3 2 (@bvand yb zb)))
   (check-pure-bitvector-term (@zero-extend (@bvxor xb (@bvand zb yb)) (bitvector 8)))
   (check-pure-bitvector-term (@sign-extend (@bvxor xb (@bvand zb yb)) (bitvector 8)))
   ))

(time (run-tests tests:pure-bitvector-terms))
(send solver shutdown)


