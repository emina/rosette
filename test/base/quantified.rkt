#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term rosette/base/core/bool)

(define-syntax-rule (check-state actual expected-value expected-asserts)
  (let-values ([(e ignore) (with-asserts expected-value)]
               [(v a) (with-asserts actual)])
    (check-equal? v e)
    (check-equal? (apply set a) (apply set expected-asserts))))

(define (check-pe op)
  (define-symbolic a boolean?)
  (define-symbolic b integer?)
  (define-symbolic c real?)
  (define-symbolic f (~> integer? real?))
  (check-exn #px"primitive solvable types" (thunk (op a #t))) ; not a list
  (check-exn #px"primitive solvable types" (thunk (op (list f) #t))) ; not solvable
  (check-exn #px"primitive solvable types" (thunk (op (list b 1) #t))) ; not a constant
  (check-exn #px"boolean\\?" (thunk (op (list) 1))) ; not boolean body
  (check-exn #px"boolean\\?" (thunk (op (list b c) 1))) ; not boolean body
  (clear-asserts!)
  (check-state (op (list a b c) #t) #t (list)) ; constant body
  (check-state (op (list a b c) #f) #f (list)) ; constant body
  (check-state (op (list) a) a (list)) ; empty list of quantified variables
  (check-state (op (list b c) (= 3 (+ b c))) (expression op (list b c) (= 3 (+ b c))) (list))
  (check-state (op (list b c) (if a #t 1)) #t (list a))
  (check-state (op (list) (if a #t 1)) #t (list a))
  (check-state (&& a (op (list b c) (= 3 (+ b c))))
               (&& a (expression op (list b c) (= 3 (+ b c)))) (list))
  (check-state (op (list b) (op (list c) (= 3 (+ b c))))
               (expression op (list b)
                           (expression op (list c)
                                       (= 3 (+ b c))))
               (list)))

(define (check-finitized op)
  (define-symbolic a boolean?)
  (define-symbolic b integer?)
  (define-symbolic c real?)
  (check-exn #px"cannot use \\(current-bitwidth 5\\) with a quantified formula"
             (thunk (solve (assert (op (list b) (op (list c) (= 3 (+ b c))))))))
  (check-exn #px"cannot use \\(current-bitwidth 5\\) with a quantified formula"
             (thunk (verify (assert (&& a (op (list b) (op (list c) (= 3 (+ b c))))))))))

(define (check-sol actual expected)
  (check-equal? (sat? actual) (sat? expected))
  (when (sat? expected)
    (check-equal? (model actual) (model expected))))

(define (check-solve)
  (define-symbolic a b c d integer?)
  (check-sol
   (solve (assert (forall (list a c) (exists (list b) (not (= (+ a c) b))))))
   (sat))
  (check-sol
   (solve (assert (exists (list c) (forall (list b) (not (= (+ b c) b))))))
   (sat))
  (check-sol
   (solve (assert (exists (list a) (forall (list b) (and (= a 0) (= (+ b c) (+ a b)))))))
   (sat (hash c 0)))
  (check-sol
   (solve (assert (exists (list a) (forall (list b) (< a (- b a))))))
   (unsat))
  (check-sol
   (solve
    (begin
      (assert (forall (list a) (exists (list b) (and (= a b) (= (+ a c) b)))))
      (assert (forall (list a) (exists (list b c) (and (not (= a b)) (= (+ a c) b)))))))
   (sat (hash c 0)))
  (check-sol
   (solve
    (begin
      (assert (forall (list a) (exists (list b) (and (= a b) (= (+ a c) b)))))
      (assert (forall (list a c) (exists (list b c) (and (not (= a b)) (= (+ a c) b)))))))
   (sat (hash c 0)))
  (check-sol
   (solve
    (begin
      (assert (forall (list a) (exists (list b) (and (= a b) (= (+ a c) b)))))
      (assert (forall (list a c) (exists (list b) (= (+ a c) b))))))
   (sat (hash c 0)))
  (check-sol
   (solve
    (begin 
      (assert (forall (list a) (exists (list b) (and (= a b) (= (+ a c) b)))))
      (assert (forall (list a) (exists (list b) (and (not (= a b)) (= (+ a c) b)))))))
   (unsat))
)

(define (check-eval)
  (define-symbolic a b c d integer?)
  (define-symbolic x y (bitvector 4))
  (let ([f (forall (list a c) (exists (list b) (not (= (+ a c) b))))])
    (check-equal? (evaluate f (solve (assert f))) f))
  (let ([f  (exists (list c) (forall (list b) (not (= (+ b c) b))))])
    (check-equal? (evaluate f (solve (assert f))) f))
  (let ([f  (exists (list a) (forall (list b) (and (= a 0) (= (+ b c) (+ a b)))))])
    (check-equal?
     (evaluate f (solve (assert f)))
     (exists (list a) (forall (list b) (and (= a 0) (= b (+ a b)))))))
  (let* ([f (forall (list a) (exists (list b) (and (= a b) (= (+ a c) b))))]
         [g (forall (list a) (exists (list b c) (and (not (= a b)) (= (+ a c) b))))]
         [s (solve (begin (assert f) (assert g)))])    
    (check-equal?
     (evaluate f s)
     (forall (list a) (exists (list b) (and (= a b) (= a b)))))
    (check-equal?
     (evaluate g s)
     g))
  (let ([f (forall (list x) (bveq (bvadd x y) x))])
    (check-equal?
     (evaluate f (solve (assert f)))
     (forall (list x) (bveq (bvadd x (bv 0 4)) x))))
)

(define (check-uninterpreted)
  (let ()
    (define-symbolic x y boolean?)
    (define-symbolic f (~> boolean? boolean? boolean?))
    (define sol (solve (assert (forall (list x y) (equal? (f x y) (or x y))))))
    (check-pred (disjoin sat? unknown?) sol)
    (when (sat? sol)
      (check-unsat (verify (assert (equal? (evaluate (f x y) sol) (or x y)))))))

  (let ()
    (define-symbolic x integer?)
    (define-symbolic f (~> integer? integer?))
    (define sol (solve (assert (forall (list x) (= x (f x))))))
    (check-pred (disjoin sat? unknown?) sol)
    (when (sat? sol)
      (check-unsat (verify (assert (equal? (evaluate (f x) sol) (identity x)))))))

  (let ()
    (define-symbolic offset integer?)
    (define-symbolic a (~> integer? integer?))
    (define-symbolic disk (~> integer? integer? integer?))
    (define sol (solve (assert (forall (list offset) (= (a offset) (disk 5 offset))))))
    (check-pred (disjoin sat? unknown?) sol)
    (when (sat? sol)
      (check-unsat (verify (assert (equal? (evaluate (a offset) sol) (evaluate (disk 5 offset) sol))))))
    )

  (let ()
    (define-symbolic offset real?)
    (define-symbolic a (~> real? real?))
    (define-symbolic disk (~> real? real? real?))
    (define sol (solve (assert (forall (list offset) (= (a offset) (disk 5.35 offset))))))
    (check-pred (disjoin sat? unknown?) sol)
    (when (sat? sol)
      (check-unsat (verify (assert (equal? (evaluate (a offset) sol) (evaluate (disk 5.35 offset) sol))))))
  )
  )

(define tests:basic
  (test-suite+
   "Basic tests for quantified formulas"
   (current-bitwidth #f)
   (check-pe exists)
   (check-pe forall)))

(define tests:finitized
  (test-suite+
   "Tests for finitization of quantified formulas"
   (current-bitwidth 5)
   (check-finitized exists)
   (check-finitized forall)))

(define tests:solving
  (test-suite+
   "Tests for solving quantified formulas"
   #:features '(qf_lia quantifiers)
   (current-bitwidth #f)
   (check-solve)))

(define tests:eval
  (test-suite+
   "Tests for evaluating quantified formulas"
   #:features '(qf_bv qf_lia quantifiers)
   (current-bitwidth #f)
   (check-eval)))

(define tests:uninterpreted
  (test-suite+
   "Tests for solving quantified formulas in the presence of uninterpreted functions"
   #:features '(qf_uf qf_lia quantifiers)
   (current-bitwidth #f)
   (check-uninterpreted)))

(module+ test
  (time (run-tests tests:basic))
  (time (run-tests tests:finitized))
  (time (run-tests tests:solving))
  (time (run-tests tests:eval))
  (time (run-tests tests:uninterpreted)))
