#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/lib/synthax "synthax-external.rkt")

(define-symbolic a b integer?)

(define-grammar (h4) [s (choose 1 2)])
(define (h5) (h4))
(define (h6 x) (choose (??) (+ x (h5))))

(define-grammar (rec x)
  [s (choose x (??) (+ x (s)))])

(define-simple-grammar (srec x)
  (choose x (??) (+ x (srec))))

(define (hrec x d) (rec x #:depth d))

(define (hsrec x d) (srec x #:depth d))

(define-grammar (LIA x y)
   [I  (choose 0 1 x y (+ (I) (I)) (* (Ic) (I)))]
   [Ic (choose 0 1 2 -1 -2)])

(define (hLIA x y d) (LIA x y #:depth d))

(define-grammar (SEQ cmd)
  [s (choose (c) (begin (c) (s)))]
  [c (choose (void) cmd)])

(define (hSEQ v d)
  (define t v)
  (SEQ (set! t (add1 t)) #:depth d)
  t)

(define-grammar (mutual z)
  [ping (choose z (+ 1 (pong)))]
  [pong (choose z (+ 2 (ping)))])

(define (add12 z d)
  (mutual z #:depth d))

(define (add21 z d)
  (mutual z #:depth d #:start pong))


(define (m1 x d) (grec x #:depth d))
(define (h1 x)   (choose 1 (choose x 3)))
(define (m2 x d) (grec (h1 x) #:depth d))

(define (grammar-or-constant x d)
  (choose (??) (mutual x #:depth d)))

(define-grammar (inc-or-dec x)
  [s   (choose (inc) (dec))]
  [inc (choose x (add1 (inc)))]
  [dec (choose x (sub1 (dec)))])

(define-grammar (times-inc-or-dec x)
  [s   (choose 1 (* (idc) (s)))]
  [idc (inc-or-dec x #:depth 3)]) ; unroll idc up to 3 times, regardless of parent depth

(define-grammar (times-inc-or-dec* x)
  [s   (choose 1 (* (idc) (s)))]
  [idc (inc-or-dec x)])           ; use the parent depth to control the expansion of idc

(define (h-times-inc-or-dec x d)
  (times-inc-or-dec x #:depth d))

(define (h-times-inc-or-dec* x d)
  (times-inc-or-dec* x #:depth d))

(define-namespace-anchor tests)
(define ns (namespace-anchor->namespace tests))

(define (verified-equal? vars impl spec)
  (or (equal? impl spec)
      (begin 
        (match-define `(,_ ... (define ,spec-h ,_ ...)) spec)
        (define consts
          (append (map term->datum vars)
                  (make-list (- (length spec-h) (length vars) 1) #f)))
        (define body `(let ([impl (lambda ,(cdr spec-h) ,@impl ,spec-h)]
                            [spec (lambda ,(cdr spec-h) ,@spec ,spec-h)])
                        (unsat?
                         (verify
                          (assert
                           (equal? (impl ,@consts) (spec ,@consts)))))))
        (eval body ns))))
  
(define-syntax-rule (check-synth vars expr expected)
  (let* ([vs (symbolics vars)]
         [sol (synthesize #:forall vs #:guarantee (assert expr))])
    (check-sat sol)
    (check-true
     (verified-equal? vs (map syntax->datum (generate-forms sol)) expected))))

(define-syntax-rule (check-unsynth vars expr)
  (with-terms
    (check-unsat (synthesize #:forall vars #:guarantee (assert expr)))))
  

(define basic-tests
  (test-suite+ "Basic grammar tests."
   (check-synth null (= 1 (h5)) '((define (h5) 1)))
   (check-unsynth null (! (|| (= (h5) 1) (= (h5) 2))))
   (check-synth null (= 4 (+ (h5) (h5))) '((define (h5) 2)))
   (check-unsynth null (= 3 (+ (h5) (h5))))
   (check-synth a (= (+ a 1) (h6 a)) '((define (h5) 1)
                                       (define (h6 x) (+ x (h5)))))
   (check-unsynth a (= (+ a a) (h6 a)))
   (check-synth a (= (+ a 4) (+ (h5) (h6 a))) '((define (h5) 2)
                                                (define (h6 x) (+ x (h5)))))))

(define one-arg-recursive-grammar-tests
  (test-suite+ "One-argument recursive grammar / sketch tests."
    (check-synth a (= -5 (hrec a 1)) '((define (hrec x d) -5)))
    (check-synth a (= -5 (hrec a 2)) '((define (hrec x d) -5)))
    (check-synth a (= -5 (hrec a 3)) '((define (hrec x d) -5)))
    (check-synth a (= a (hrec a 1)) '((define (hrec x d) x)))
    (check-synth a (= (+ a 3) (hrec a 1)) '((define (hrec x d) (+ x 3))))
    (check-synth a (= (* a 3) (hrec a 2)) '((define (hrec x d) (+ x (+ x x)))))
    (check-synth a (= a (hsrec a 1)) '((define (hsrec x d) x)))
    (check-synth a (= (* a 3) (hsrec a 2)) '((define (hsrec x d) (+ x (+ x x)))))))
  
(define two-arg-recursive-grammar-tests
  (test-suite+ "Two-argument recursive grammar tests."
    (check-synth (list a b) (= 0 (hLIA a b 0)) '((define (hLIA x y d) 0)))
    (check-synth (list a b) (= b (hLIA a b 0)) '((define (hLIA x y d) y)))
    (check-unsynth (list a b) (= (+ a a) (hLIA a b 0)))
    (check-synth (list a b) (= (+ a b) (hLIA a b 1)) '((define (hLIA x y d) (+ y x))))
    (check-unsynth (list a b) (= (- a 1) (hLIA a b 1)))
    (check-synth (list a b) (= (- a 1) (hLIA a b 2)) '((define (hLIA x y d) (+ (* -1 1) x))))
    (check-synth (list a b) (= (- (* 2 a) (* 2 b)) (hLIA a b 2)) '((define (hLIA x y d) (+ (* -2 y) (+ x x)))))
    (check-unsynth (list a b) (= (* a b) (hLIA a b 1)))))

(define mutually-recursive-grammar-tests
  (test-suite+ "Mutually recursive grammar tests."
    (check-synth a (= (+ a 4) (add12 a 3)) '((define (add12 z d) (+ 1 (+ 2 (+ 1 z))))))
    (check-synth a (= (+ a 5) (add21 a 3)) '((define (add21 z d) (+ 2 (+ 1 (+ 2 z))))))
    (check-synth a (let ([a12 (add12 a 2)] [a21 (add21 a 2)])
                        (assert (! (= a12 a)))
                        (assert (= a12 a21)))
                 '((define (add12 z d) (+ 1 (+ 2 z)))
                   (define (add21 z d) (+ 2 (+ 1 z)))))
                 
    ))

(define side-effects-tests
  (test-suite+ "Tests for recursive grammars and side effects."
    (check-synth a (= a (hSEQ a 1)) '((define (hSEQ v d) (define t v) (void) t)))
    (check-synth a (= (+ a 1) (hSEQ a 1)) '((define (hSEQ v d) (define t v) (set! t (add1 t)) t)))
    (check-synth a (= (+ a 2) (hSEQ a 2)) '((define (hSEQ v d)
                                              (define t v)
                                              (begin (set! t (add1 t)) (set! t (add1 t)))
                                              t)))
    (check-synth a (= (+ a 5) (hSEQ a 5)) '((define (hSEQ v d)
                                              (define t v)
                                              (begin
                                                (set! t (add1 t))
                                                (begin
                                                  (set! t (add1 t))
                                                  (begin (set! t (add1 t)) (begin (set! t (add1 t)) (set! t (add1 t))))))
                                              t)))))

(define imported-grammar-tests
  (test-suite+ "Tests that use grammars defined in imported modules."
    (check-synth a (= -5 (m1 a 1)) '((define (c0) -5)
                                     (define (m1 x d) (c0))))
    (check-synth a (= (+ a 2) (m2 a 1)) '((define (c0) 2)
                                          (define (h1 x) x)
                                          (define (m2 x d) (+ (h1 x) (c0)))))))

(define composed-grammar-tests
  (test-suite+ "Tests that compose grammars."
    (check-synth a (= 11 (grammar-or-constant a 1)) '((define (grammar-or-constant x d) 11)))
    (check-synth a (= a (grammar-or-constant a 1))  '((define (grammar-or-constant x d) x)))
    (check-synth a (= (+ a 3) (grammar-or-constant a 2))
                 '((define (grammar-or-constant x d) (+ 1 (+ 2 x)))))
    (check-synth a (= (* (+ a 2) (- a 1)) (h-times-inc-or-dec a 2))
                 '((define (h-times-inc-or-dec x d)
                     (* (sub1 x) (* (add1 (add1 x)) 1)))))
    (check-unsynth a (= (* (+ a 2) (- a 1)) (h-times-inc-or-dec* a 2)))
    (check-synth a (= (* (+ a 2) (- a 1)) (h-times-inc-or-dec* a 5))
                 '((define (h-times-inc-or-dec* x d)
                     (* (add1 (add1 x)) (* (sub1 x) 1)))))
    ))

(module+ test
  (time (run-tests basic-tests))
  (time (run-tests one-arg-recursive-grammar-tests))
  (time (run-tests two-arg-recursive-grammar-tests))
  (time (run-tests mutually-recursive-grammar-tests))
  (time (run-tests side-effects-tests))
  (time (run-tests imported-grammar-tests)) 
  (time (run-tests composed-grammar-tests)))