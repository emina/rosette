#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term rosette/base/core/uninterpreted)


(define (check-equivalent impl spec)
  (define inputs (for/list ([t (function-domain impl)])
                   (define-symbolic* in t)
                   t))
  (verify (assert (equal? (apply impl inputs) (apply spec inputs)))))

(define (test-equivalent impl io-pairs)
  (for ([p io-pairs])
    (check = (apply impl (car p)) (cdr p))))

(define (check-not)
  (define-symbolic ¬ (-> boolean? boolean?))
  (define sol (solve (assert (equal? (¬ #t) #f))
                     (assert (equal? (¬ #f) #t))))
  (check-equivalent (sol ¬) !))

(define (check-and)
  (define-symbolic ∧ (-> boolean? boolean? boolean?))
  (define sol
    (solve (assert (equal? (∧ #t #t) #t))
           (assert (equal? (∧ #t #f) #f))
           (assert (equal? (∧ #f #t) #f))
           (assert (equal? (∧ #f #f) #f))))
  (check-equivalent (sol ∧) &&))

(define (check-boolean-mixed)
  (define-symbolic ∧ (-> boolean? boolean? boolean?))
  (define-symbolic a b boolean?)
  (define sol
    (solve (assert (equal? a #t))
           (assert (equal? b #f))
           (assert (equal? (∧ a a) a))
           (assert (equal? (∧ a b) b))
           (assert (equal? (∧ b a) b))
           (assert (equal? (∧ b (&& (! a) b)) b))))
  (check-equivalent (sol ∧) &&))
  
(define (check-bvnot)
  (define-symbolic ~ (-> (bitvector 4) (bitvector 4)))
  (define sol
    (solve (for ([i (in-range -8 8)])
             (assert (equal? (~ (bv i 4)) (bvnot (bv i 4)))))))
  (check-equivalent (sol ~) bvnot))

(define (check-bvadd)
  (define-symbolic ⊕ (-> (bitvector 4) (bitvector 4) (bitvector 4)))
  (define sol
    (solve (for* ([i (in-range -8 8)][j (in-range -8 8)])
             (assert (equal? (⊕ (bv i 4) (bv j 4)) (bvadd (bv i 4) (bv j 4)))))))
  (check-equivalent (sol ⊕) bvadd))

; Basic tests for UFs over booleans.
(define (check-boolean?)
  (check-not)
  (check-and)
  (check-boolean-mixed))

; Basic tests for UFs over bitvectors.
(define (check-bitvector?)
  (check-bvnot)
  (check-bvadd))

; Basic tests for UFs over integers and reals.
(define (check-int-real)
  (define-symbolic ⊕ (-> integer? integer? real?))
  (define-symbolic x integer?)
  (define sol (solve (assert (= x 1))
                     (assert (= (⊕ x 1) (+ x 1)))
                     (assert (= (⊕ 2 x) 3))
                     (assert (= (⊕ 2 (+ x 2)) 6))))
  (test-equivalent (sol ⊕) '(((1 1) . 2) ((2 1) . 3) ((2 3) . 6))))

(define (check-int-real-no-finite-solution)
  (define-symbolic f (-> boolean? real?))
  (check-pred unsat? (solve (assert (= (f #f) .5)))))

(define tests:basic
  (test-suite+
   "UF tests with no finitization"
   (current-bitwidth #f)
   (check-boolean?)
   (check-bitvector?)
   (check-int-real)
   ))


(define tests:finitized
  (test-suite+
   "UF tests with finitization"
   (current-bitwidth 5)
   (check-boolean?)
   (check-bitvector?)
   (check-int-real)
   (check-int-real-no-finite-solution)
   ))

   

(time (run-tests tests:basic))
(time (run-tests tests:finitized))