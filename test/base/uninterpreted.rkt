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
  (define sol
    (solve (begin
             (assert (equal? (¬ #t) #f))
             (assert (equal? (¬ #f) #t)))))
  (check-equivalent (sol ¬) !))

(define (check-and)
  (define-symbolic ∧ (~> boolean? boolean? boolean?))
  (define sol
    (solve
     (begin
       (assert (equal? (∧ #t #t) #t))
       (assert (equal? (∧ #t #f) #f))
       (assert (equal? (∧ #f #t) #f))
       (assert (equal? (∧ #f #f) #f)))))
  (check-equivalent (sol ∧) &&))

(define (check-boolean-mixed)
  (define-symbolic ∧ (~> boolean? boolean? boolean?))
  (define-symbolic a b boolean?)
  (define sol
    (solve
     (begin
       (assert (equal? a #t))
       (assert (equal? b #f))
       (assert (equal? (∧ a a) a))
       (assert (equal? (∧ a b) b))
       (assert (equal? (∧ b a) b))
       (assert (equal? (∧ b (&& (! a) b)) b)))))
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
  (define sol
    (solve
     (begin
       (assert (= x 1))
       (assert (= (⊕ x 1) (+ x 1)))
       (assert (= (⊕ 2 x) 3))
       (assert (= (⊕ 2 (+ x 2)) 6)))))
  (test-equivalent (sol ⊕) '(((1 1) . 2) ((2 1) . 3) ((2 3) . 6))))

(define (check-finitization-disallowed)
  (current-bitwidth 5)
  (define-symbolic f (~> boolean? real?))
  (check-exn #px"finitize.*" (thunk (solve (assert (= (f #f) .5))))))

; Basic tests for booleans and 1-bit bitvectors (some solvers conflate them)
(define (check-bitvector-booleans)
  (define-symbolic even (~> (bitvector 2) boolean?))
  (define sol
    (solve (for ([i (in-range -2 2)])
             (assert (equal? (even (bv i 2)) (even? i))))))
  (check-sat sol)
  (check-equal? (evaluate (even (bv -2 2)) sol) #t)
  
  (define-symbolic f (~> boolean? (bitvector 2)))
  (define solf
    (solve (assert (and (equal? (f #t) (bv 0 2))
                        (equal? (f #f) (bv -2 2))))))
  (check-sat solf)
  (check-equal? (evaluate (f #t) solf) (bv 0 2))
  (check-equal? (evaluate (f #f) solf) (bv -2 2))
  (check-exn #px"boolean\\?" (thunk ((evaluate f solf) (bv 0 1)))))
    

(define-syntax-rule (check-state actual expected)
  (let ([re (with-vc expected)]
        [ra (with-vc actual)])
    (check-equal? (result-value ra) (result-value re))
    (check-equal? (result-state ra) (result-state re))))

(define (check-types)
  (define-symbolic a boolean?)
  (define-symbolic b integer?)
  (define-symbolic c real?)
  (define-symbolic f (~> boolean? boolean?))
  (define-symbolic g (~> integer? integer?))
  (define-symbolic h (~> real? real?))
  (check-exn #px"boolean\\?" (thunk (f 3)))
  (clear-vc!)
  (check-exn #px"boolean\\?" (thunk (f b)))
  (clear-vc!)
  (check-exn #px"boolean\\?" (thunk (f (if a b c))))
  (clear-vc!)
  (check-exn #px"integer\\?" (thunk (g a)))
  (clear-vc!)
  (check-exn #px"integer\\?" (thunk (g (if a 'b 'c))))
  (clear-vc!)
  (check-exn #px"real\\?" (thunk (h a)))
  (clear-vc!)
  (check-exn #px"real\\?" (thunk (h (if a 'b 'c))))
  (clear-vc!)
  (check-exn #px"arity" (thunk (f)))
  (clear-vc!)
  (check-state (g b) (expression @app g b))
  (check-state (g (if a b 'b)) (begin (assert a) (expression @app g b)))
  (check-state (g (if a b c)) (expression @app g (type-cast integer? (if a b c))))  
  (check-state (g c) (begin (assert (int? c)) (expression @app g (real->integer c))))
  (check-state (h c) (expression @app h c))
  (check-state (h b) (expression @app h (integer->real b)))
  (check-state (h (if a b 'b)) (begin (assert a) (expression @app h (integer->real b))))
  (check-state (h (if a b c)) (expression @app h (type-cast real? (if a b c)))))
  

(define tests:basic
  (test-suite+
   "UF basic tests"
   #:features '(qf_uf qf_bv)
   (current-bitwidth #f)
   (check-boolean?)
   (check-bitvector?)
   (check-bitvector-booleans)))

(define tests:unfinitized
  (test-suite+
   "UF tests without finitization"
   #:features '(qf_uf qf_lia qf_lra)
   (current-bitwidth #f)
   (check-int-real)))

(define tests:finitized
  (test-suite+
   "UF tests with finitization"
   (check-finitization-disallowed)
   ))

(define tests:lifted
  (test-suite+
   "UF tests for lifted applications"
   (check-types)))

(module+ test
  (time (run-tests tests:basic))
  (time (run-tests tests:unfinitized))
  (time (run-tests tests:finitized))
  (time (run-tests tests:lifted)))
