#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/base/core/term rosette/base/core/function)


(define (check-equivalent impl spec)
  (define inputs (for/list ([t (function-domain (get-type impl))])
                   (define-symbolic* in t)
                   t))
  (verify (assert (equal? (apply impl inputs) (apply spec inputs)))))

(define (test-equivalent impl io-pairs)
  (for ([p io-pairs])
    (check = (apply impl (car p)) (cdr p))))

(define (check-not)
  (define-symbolic ¬ (~> boolean? boolean?))
  (define sol (solve (assert (equal? (¬ #t) #f))
                     (assert (equal? (¬ #f) #t))))
  (check-equivalent (sol ¬) !))

(define (check-and)
  (define-symbolic ∧ (~> boolean? boolean? boolean?))
  (define sol
    (solve (assert (equal? (∧ #t #t) #t))
           (assert (equal? (∧ #t #f) #f))
           (assert (equal? (∧ #f #t) #f))
           (assert (equal? (∧ #f #f) #f))))
  (check-equivalent (sol ∧) &&))

(define (check-boolean-mixed)
  (define-symbolic ∧ (~> boolean? boolean? boolean?))
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
  (define-symbolic ~ (~> (bitvector 4) (bitvector 4)))
  (define sol
    (solve (for ([i (in-range -8 8)])
             (assert (equal? (~ (bv i 4)) (bvnot (bv i 4)))))))
  (check-equivalent (sol ~) bvnot))

(define (check-bvadd)
  (define-symbolic ⊕ (~> (bitvector 4) (bitvector 4) (bitvector 4)))
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
  (define-symbolic ⊕ (~> integer? integer? real?))
  (define-symbolic x integer?)
  (define sol (solve (assert (= x 1))
                     (assert (= (⊕ x 1) (+ x 1)))
                     (assert (= (⊕ 2 x) 3))
                     (assert (= (⊕ 2 (+ x 2)) 6))))
  (test-equivalent (sol ⊕) '(((1 1) . 2) ((2 1) . 3) ((2 3) . 6))))

(define (check-finitization-disallowed)
  (current-bitwidth 5)
  (define-symbolic f (~> boolean? real?))
  (check-exn #px"finitize.*" (thunk (solve (assert (= (f #f) .5))))))

(define-syntax-rule (check-state actual expected-value expected-asserts)
  (let-values ([(e ignore) (with-asserts expected-value)]
               [(v a) (with-asserts actual)])
    (check-equal? v e)
    (check-equal? (apply set a) (apply set expected-asserts))))

(define (check-types)
  (define-symbolic a boolean?)
  (define-symbolic b integer?)
  (define-symbolic c real?)
  (define-symbolic f (~> boolean? boolean?))
  (define-symbolic g (~> integer? integer?))
  (define-symbolic h (~> real? real?))
  (check-exn #px"boolean\\?" (thunk (f 3)))
  (check-exn #px"boolean\\?" (thunk (f b)))
  (check-exn #px"boolean\\?" (thunk (f (if a b c))))
  (check-exn #px"integer\\?" (thunk (g a)))
  (check-exn #px"integer\\?" (thunk (g (if a 'b 'c))))
  (check-exn #px"real\\?" (thunk (h a)))
  (check-exn #px"real\\?" (thunk (h (if a 'b 'c))))
  (check-exn #px"arity" (thunk (f)))
  (clear-asserts!)
  (check-state (g b) (expression @app g b) '())
  (check-state (g (if a b 'b)) (expression @app g b) (list a))
  (check-state (g (if a b c)) (expression @app g (type-cast integer? (if a b c))) (list (|| a (&& (! a) (int? c)))))
  (check-state (g c) (expression @app g (real->integer c)) (list (int? c)))
  (check-state (h c) (expression @app h c) '())
  (check-state (h b) (expression @app h (integer->real b)) '())
  (check-state (h (if a b 'b)) (expression @app h (integer->real b)) (list a))
  (check-state (h (if a b c)) (expression @app h (type-cast real? (if a b c))) '())
  )
  

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
   (check-finitization-disallowed)
   ))

(define tests:lifted
  (test-suite+
   "UF tests for lifted applications"
   (check-types)))

(time (run-tests tests:basic))
(time (run-tests tests:finitized))
(time (run-tests tests:lifted))