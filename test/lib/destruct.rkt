#lang rosette/safe

(require rackunit rackunit/text-ui rosette/lib/roseunit
         rosette/lib/angelic
         rosette/lib/destruct
         (only-in rosette/base/core/polymorphic ite)
         (only-in racket open-input-string with-output-to-string ==)
         syntax/macro-testing)

(define building-synthesizer-tests
  (test-suite+
   "Tests from ``Building a Program Synthesizer''"
   ;; https://www.cs.utexas.edu/~bornholt/post/building-synthesizer.html

   ; Syntax for our simple DSL
   (struct plus (left right) #:transparent)
   (struct mul (left right) #:transparent)
   (struct square (arg) #:transparent)

   (define-symbolic y integer?)

   ; Interpreter for our DSL.
   ; We just recurse on the program's syntax using pattern matching.
   (define (interpret p)
     (destruct p
       [(plus a b)  (+ (interpret a) (interpret b))]
       [(mul a b)   (* (interpret a) (interpret b))]
       [(square a)  (expt (interpret a) 2)]
       [_ p]))

   ; (plus (square 7) 3) evaluates to 52.
   (check-equal? (interpret (plus (square 7) 3)) 52)

   ; So we can search for a `y` that makes (y+2)^2 = 25
   (let ([sol (solve (assert (= (interpret (square (plus y 2))) 25)))])
     (check-match (evaluate y sol) (or 3 -7)))


   (define-symbolic x c integer?)

   ; Find a `c` such that c*x = x+x for *every* x.
   (let ([sol (synthesize
               #:forall (list x)
               #:guarantee (assert (= (interpret (mul c x)) (+ x x))))])
     (check-equal? (evaluate c sol) 2))


   ;; to make the below testable, we will force the solution to be
   ;; 10x = x + 9x

   ; Create an unknown expression -- one that can evaluate to several
   ; possible values.
   (define (??expr terminals)
     (define a (apply choose* terminals))
     (define b (apply choose* terminals))
     (choose* (plus a b)
              (mul a b)
              (square a)
              a))


   ; Create a sketch representing all programs of the form (plus ?? ??),
   ; where the ??s are unknown expressions created by ??expr.
   (define-symbolic p q integer?)
   (define sketch
     (plus x (??expr (list x p q))))

   ; Solve the sketch to find a program equivalent to 10*x,
   ; but of the form (plus ?? ??). Save the resulting model.
   (define M
     (synthesize
      #:forall (list x)
      #:guarantee (assert (= (interpret sketch) (interpret (mul 10 x))))))


   ; Substitute the bindings in M into the sketch to get back the
   ; synthesized program.
   (define s (with-output-to-string (λ () (println (evaluate sketch M)))))
   (check-match (read (open-input-string s))
                (or '(plus x (mul x 9)) '(plus x (mul 9 x))))))

(define unit-tests
  (test-suite+
   "Tests for rosette/lib/destruct"

   ;; should not allow complex subpattern
   (check-exn #px"a sub-pattern"
              (λ () (convert-syntax-error
                     (destruct 1 [(list (list)) 1]))))

   ;; should not support #:when
   (check-exn #px"expected expression"
              (λ () (convert-syntax-error
                     (destruct 1 [_ #:when #t 1]))))

   ;; should not treat => specially
   (check-exn #px"unbound-id: unbound identifier"
              (λ () (convert-syntax-error
                     (destruct 1 [_ (=> unbound-id) (unbound-id) 1]))))

   ;; should not support and, or, not
   ;; (we can add them back later if we really want to support them)
   (check-exn #px"a head pattern"
              (λ () (convert-syntax-error
                     (destruct 1 [(and _ _) 1]))))

   (check-exn #px"a: duplicate binding identifier"
              (λ () (convert-syntax-error
                     (destruct (list 1 1) [(list a a) a]))))

   (check-exn #px"a: duplicate binding identifier"
              (λ () (convert-syntax-error
                     (destruct* (1 1) [(a a) a]))))

   ;; test _ and ...
   (check-equal? (destruct '(1 2 3 4) [(list _ x ... _) x])
                 '(2 3))

   ;; test ..0
   (check-equal? (destruct '() [(list x ..0 y ..0) (append x y)])
                 '())

   (define-symbolic b boolean?)

   ;; test struct
   (struct f (x))
   (struct g (x))

   (check-match (destruct (if b (f 10) (g 100))
                  [(f x) (add1 x)]
                  [(g x) (sub1 x)])
                (expression (== ite) b 11 99))

   (check-match (destruct (if b (f 10) (f 100)) [(f x) (add1 x)])
                (expression (== ite) b 11 101))

   (struct f2 (x) #:transparent)
   (struct g2 (x) #:transparent)

   (check-match (destruct (if b (f2 10) (g2 100))
                  [(f2 x) (add1 x)]
                  [(g2 x) (sub1 x)])
                (expression (== ite) b 11 99))

   (check-match (destruct (if b (f2 10) (f2 100)) [(f2 x) (add1 x)])
                (expression (== ite) b 11 101))

   ;; test list
   (check-match (destruct (if b (list 1) (list 2)) [(list x) x])
                (expression (== ite) b 1 2))

   ;; test vector
   (check-match (destruct (if b (vector 1) (vector 2)) [(vector x) x])
                (expression (== ite) b 1 2))

   ;; test box
   (check-match (destruct (if b (box 1) (box 2)) [(box x) x])
                (expression (== ite) b 1 2))

   ;; test internal definition context
   (check-equal? (destruct 1
                   [_ (define x 42)
                       (set! x (add1 x))
                       x])
                 43)))

(module+ test
  (time (run-tests building-synthesizer-tests))
  (time (run-tests unit-tests)))
