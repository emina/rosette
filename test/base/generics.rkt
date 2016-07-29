#lang rosette

(require rackunit rackunit/text-ui rosette/lib/roseunit)

(define-generics simple
  (answer simple x))

(struct life ()
  #:methods gen:simple
  [(define (answer self x) 42)])

(define-generics evaluator
  (evaluat evaluator [env])
  (f evaluator . args)
  #:defined-predicate has-methods?
  #:fallbacks
  [(define (f self . args)
     (if (null? args)
         0
         (+ (car args) (apply f self (cdr args)))))]
  #:fast-defaults
  ([number?
    (define (evaluat thing [env #hash()]) thing)
    (define (f self . args) self)]
   [symbol?
    (define (evaluat thing [env #hash()])
      (hash-ref env thing))])
  #:derive-property prop:procedure evaluat
  #:defaults
  ([pair?
    (define (evaluat thing [env #hash()])
      (if (not (pair? thing))
          thing
          (apply (car thing) (map (lambda (x) (evaluat x env))
                                  (cdr thing)))))]))

(struct Adder (x y)
  #:methods gen:evaluator
  [(define/generic super-evaluat evaluat)
   (define (evaluat self [env #hash()])
     (+ (super-evaluat (Adder-x self) env)
        (super-evaluat (Adder-y self) env)))])

(struct Multiplier (x y)
  #:methods gen:evaluator
  [(define/generic super-evaluat evaluat)
   (define (evaluat self [env #hash()])
     (* (super-evaluat (Multiplier-x self) env)
        (super-evaluat (Multiplier-y self) env)))])

;; An example where we don't use define/generic but still have recursion
(struct Applier (fn x y)
  #:methods gen:evaluator
  [(define (evaluat self [env #hash()])
     (if (number? self)
         self
         ((Applier-fn self)
          (evaluat (Applier-x self) env)
          (evaluat (Applier-y self) env))))])

(define gen-concrete-tests
  (test-suite+
   "Concrete tests for define-generics"

   ;; Generic function evaluation
   (check-equal? (answer (life) 3) 42)
   (check-equal? (evaluat (Adder (Multiplier 'x 3) (list + 2 3))
                           #hash((x . 2)))
                 11)

   (define eleven (Applier + (Applier * 2 3) 5))

   ;; Tests for #:defined-predicate
   (check-false (has-methods? eleven 'evaluat 'f))
   (check-true (has-methods? 11 'evaluat 'f))

   ;; Tests for optional arguments and variadic function
   (check-equal? (evaluat eleven) 11)
   (check-equal? (f eleven 2 4 6 8 10) 30)

   ;; Tests for #:derive-property
   (check-equal? (eleven) 11)))

(define gen-symbolic-tests
  (test-suite+
   "Symbolic tests for define-generics"
   (define-symbolic b boolean?)

   ;; Tests for generic function evaluation with optional arguments
   (define eleven-or-one
     (evaluat (Adder (Multiplier 'x (if b 3 0))
                      (if b (list + 2 3) (list - 3 2)))
              #hash((x . 2))))
   (define ten-or-seven
     (evaluat (if b
                  (Applier * (if b (Applier + 2 (if b 3 4)) 8) 2)
                  (Applier + 3 4))))

   
   (define sixteen-or-seven-obj
     (if b
         (Multiplier (if b (Adder 3 (if b 1 'x)) (Multiplier 2 2))
                     'y)
         (Adder 3 'y)))
   
   ;; Tests for #:defined-predicate and #:derive-property
   (define has-evaluat
     (has-methods? sixteen-or-seven-obj 'evaluat))
   (define sixteen-or-seven
     (sixteen-or-seven-obj #hash((x . 2) (y . 4))))
   (define adder-or-number
     (if b (Adder 1 2) 4))
   (define three-or-four (evaluat adder-or-number))
   (define has-both-methods?
     (has-methods? adder-or-number 'evaluat 'f))

   ;; Tests for #:defaults and #:fast-defaults
   (define four-or-fifteen
     (evaluat (if b
                  (list * (if b 2 3) (if b 2 10))
                  (list + (if b 1 2) (if b 2 4) 4 5))))

   ;; Tests for #:fallbacks
   (define multiplier-or-number (if b (Multiplier 10 20) 7))
   (define fourteen-or-seven
     (f multiplier-or-number (if b 3 4) 5 (if b 6 7)))

   (define solution (solve (assert (= three-or-four 3))))
   (check-sat solution)
   (check-equal? (evaluate eleven-or-one solution) 11)
   (check-equal? (evaluate ten-or-seven solution) 10)
   (check-true (evaluate has-evaluat solution))
   (check-equal? (evaluate sixteen-or-seven solution) 16)
   (check-equal? (evaluate three-or-four solution) 3)
   (check-false (evaluate has-both-methods? solution))
   (check-equal? (evaluate four-or-fifteen solution) 4)
   (check-equal? (evaluate fourteen-or-seven solution) 14)))

(time (run-tests gen-concrete-tests))
(time (run-tests gen-symbolic-tests))
