#lang racket

(require rackunit/text-ui rackunit rosette/lib/roseunit "solver.rkt"
         (only-in "vc.rkt" check-vc-eqv)
         (only-in "store.rkt" check-store))
(require rosette/base/core/eval rosette/base/core/store rosette/base/core/exn rosette/base/core/result)
(require rosette/base/adt/box)
(require (only-in rosette unsat?)
         (only-in rosette/base/form/define define-symbolic)
         (only-in rosette/base/core/bool
                  @boolean? @true? && || ! => <=>
                  vc clear-vc! with-vc merge-vc!
                  $assume $assert @assume @assert
                  vc-true vc-true?
                  vc-assumes vc-asserts)
         (only-in rosette/base/core/real @integer? @= @<)
         (only-in rosette/base/core/merge merge merge*))

(define (vc-eqv? actual assumes asserts)
  (unsat? (solve (! (&&  (<=> (vc-assumes actual) assumes) (<=> (vc-asserts actual) asserts))))))

(define (int-eqv? actual expected)
  (unsat? (solve (! (@= actual expected)))))

(define-syntax-rule (check-normal actual e-val e-store e-assumes e-asserts)
  (begin
    (match-define (normal (normal v st) sp) actual)
    (check-equal? v e-val)
    (check-store st e-store)
    (check-true (vc-eqv? sp e-assumes e-asserts))))

(define-syntax-rule (check-failed actual e-exn? e-assumes e-asserts)
  (begin
    (match-define (failed ex sp) actual)
    (check-pred e-exn? ex) 
    (check-true (vc-eqv? sp e-assumes e-asserts))))

(define (check-eval-assuming)
  (define-symbolic g a b @boolean?)
  (define x (@box 3))
  ;---------------------------;
  (check-normal (eval-assuming #t (const 1))
             1 null #t #t)
  (check-normal (eval-assuming g (const 1))
             1 null g #t)
  (check-normal (eval-assuming g (lambda () (@set-box! x 2)))
             (void) `((,x 0 2)) g #t)
  (check-normal (eval-assuming g (lambda () (@set-box! x 4) (@assert a)))
             (void) `((,x 0 4)) g (=> g a))
  (check-normal (eval-assuming g (lambda () (@set-box! x 5) (@assert a) (@assume b)))
             (void) `((,x 0 5)) (&& g (=> a b)) (=> g a))
  ;---------------------------;
  (check-failed (eval-assuming #f (const 1))
              exn:fail:svm:assume:core? #f #t)
  (check-failed (eval-assuming g (lambda () (@set-box! x 2) (@assume (! g))))
              exn:fail:svm:assume:user? #f #t)
  (check-failed (eval-assuming g (lambda () (@set-box! x 4) ($assume (! g))))
              exn:fail:svm:assume:core? #f #t)
  (check-failed (eval-assuming g (lambda () (@set-box! x 5) (@assert #f)))
              exn:fail:svm:assert:user? g (! g))
  (check-failed (eval-assuming g (lambda () (@set-box! x 6) ($assert #f)))
              exn:fail:svm:assert:core? g (! g))
  (check-failed (eval-assuming g (lambda () (@set-box! x 7) (1)))
              exn:fail:svm:assert:err? g (! g))
  ;---------------------------;
  (check-equal? (@unbox x) 3)
  (check-pred vc-true? (vc)))

(define-syntax-rule (check-vc-and-cell e-assumes e-asserts cell e-cell-val)
  (begin
    (check-vc-eqv e-assumes e-asserts)
    (check-true (int-eqv? (@unbox cell) e-cell-val))
    (clear-vc!)
    (@set-box! cell 1)))

(define (check-eval-guarded-0)
  (define-symbolic g a b c @boolean?)
  (define x (@box 1))
  ;---------------------------;
  (check-equal? (eval-guarded! (list #t) (list (thunk 1))) 1)
  (check-vc-and-cell #t #t x 1)
  (check-equal? (eval-guarded! (list g) (list (thunk 1))) 1)
  (check-vc-and-cell #t #t x 1)
  (eval-guarded! (list g) (list (lambda () (@set-box! x 2))))
  (check-vc-and-cell #t #t x 2)
  (eval-guarded! (list g) (list (lambda () (@set-box! x 4) (@assert a))))
  (check-vc-and-cell #t (=> g a) x 4)
  (eval-guarded! (list g) (list (lambda () (@set-box! x 5) (@assert a) (@assume b))))
  (check-vc-and-cell (=> g (=> a b)) (=> g a) x 5)
  ;---------------------------;
  (@assume c)
  (check-exn exn:fail:svm:merge? (thunk (eval-guarded! null null)))
  (check-vc-and-cell c #t x 1)
  (@assume c)
  (check-exn exn:fail:svm:merge? (thunk (eval-guarded! (list #f) (list (const 1)))))
  (check-vc-and-cell c #t x 1)
  (check-exn exn:fail:svm:merge? (thunk (eval-guarded! (list g) (list (lambda () (@set-box! x 2) (@assume (! g)))))))
  (check-vc-and-cell (! g) #t x 1)
  (check-exn exn:fail:svm:merge? (thunk (eval-guarded! (list g) (list (lambda () (@set-box! x 3) (@assert #f))))))
  (check-vc-and-cell #t (! g) x 1)
  (check-exn exn:fail:svm:merge? (thunk (eval-guarded! (list g) (list (lambda () (@set-box! x 3) (1))))))
  (check-vc-and-cell #t (! g) x 1))

(define (check-eval-guarded-1)
  (define-symbolic g a b c @boolean?)
  (define-symbolic i @integer?)
  (define !g (! g))
  (define x (@box 1))
  (define (λ-set v [r v]) (lambda () (@set-box! x v) r))
  (define (λ-set-assume v a [r v]) (lambda () (@set-box! x v) (@assume a) r))
  (define (λ-set-assert v a [r v]) (lambda () (@set-box! x v) (@assert a) r))
  (define (λ-set-err v)      (lambda () (@set-box! x v) (1) v))
  ;---------------------------;
  (check-equal? (eval-guarded! (list #t #f) (list void void)) (void))
  (check-vc-and-cell #t #t x 1)
  (check-equal? (eval-guarded! (list #t #f) (list (const 1) (const 2))) 1)
  (check-vc-and-cell #t #t x 1)
  (check-equal? (eval-guarded! (list #f #t) (list (const 1) (const 2))) 2)
  (check-vc-and-cell #t #t x 1)
  (check-equal? (eval-guarded! (list #t #f) (list (λ-set 2) (λ-set 3))) 2)
  (check-vc-and-cell #t #t x 2)
  (check-equal? (eval-guarded! (list #t #f) (list (λ-set 4) (λ-set-assume 3 #f))) 4)
  (check-vc-and-cell #t #t x 4)
  (check-equal? (eval-guarded! (list #t #f) (list (λ-set 4) (λ-set-assert 3 #f))) 4)
  (check-vc-and-cell #t #t x 4)
  (check-equal? (eval-guarded! (list #t #f) (list (λ-set 4) (λ-set-err 3))) 4)
  (check-vc-and-cell #t #t x 4)
  (check-exn exn:fail:svm:assume:core? ; fails during merge-vc!
     (thunk (eval-guarded! (list #t #f) (list (λ-set-assume 2 #f) (λ-set 3)))))
  (check-vc-and-cell #f #t x 1)
  (check-exn exn:fail:svm:assert:core? ; fails during merge-vc!
     (thunk (eval-guarded! (list #t #f) (list (λ-set-assert 2 #f) (λ-set 3)))))
  (check-vc-and-cell #t #f x 1)
  (check-equal? (eval-guarded! (list #t #f) (list (λ-set-assume 4 g) (λ-set-err 3))) 4)
  (check-vc-and-cell g #t x 4)
  (check-equal? (eval-guarded! (list #t #f) (list (λ-set-assert 4 g) (λ-set-err 3))) 4)
  (check-vc-and-cell #t g x 4)
  ;---------------------------;
  (check-equal? (eval-guarded! (list g !g) (list void void)) (void))
  (check-vc-and-cell #t #t x 1)
  (check-equal? (eval-guarded! (list g !g) (list (const 1) (const 2))) (merge g 1 2))
  (check-vc-and-cell #t #t x 1)
  (check-equal? (eval-guarded! (list g !g) (list (λ-set 2 1) (const 2))) (merge g 1 2))
  (check-vc-and-cell #t #t x (merge g 2 1))
  (check-equal? (eval-guarded! (list g !g) (list (λ-set 2 1) (λ-set 3 2))) (merge g 1 2))
  (check-vc-and-cell #t #t x (merge g 2 3))
  (check-equal? (eval-guarded! (list g !g) (list (λ-set-assume 2 #f) (λ-set 3 2))) 2)
  (check-vc-and-cell (! g) #t x 3)
  (check-equal? (eval-guarded! (list g !g) (list (λ-set-assert 2 #f) (λ-set 3 2))) 2)
  (check-vc-and-cell #t (! g) x 3)
  (check-equal? (eval-guarded! (list g !g) (list (λ-set-err 2) (λ-set 3 2))) 2)
  (check-vc-and-cell #t (! g) x 3)
  (check-exn exn:fail:svm:assume:core? ; fails during merge-vc!
    (thunk (eval-guarded! (list g !g) (list (λ-set-assume 2 #f) (λ-set-assume 3 #f)))))
  (check-vc-and-cell #f #t x 1)
  (check-exn exn:fail:svm:assert:core? ; fails during merge-vc!
    (thunk (eval-guarded! (list g !g) (list (λ-set-assert 2 #f) (λ-set-assert 3 #f)))))
  (check-vc-and-cell #t #f x 1)
  (check-exn exn:fail:svm:merge? 
    (thunk (eval-guarded! (list g !g) (list (λ-set-assume 2 #f) (λ-set-assert 3 #f)))))
  (check-vc-and-cell (! g) g x 1)
  (check-exn exn:fail:svm:merge?  
    (thunk (eval-guarded! (list g !g) (list (λ-set-assert 2 #f) (λ-set-assume 3 #f)))))
  (check-vc-and-cell g (! g) x 1)
  ;---------------------------;
  (define-values (i- i0 i+) (values (@< i 0) (@= i 0) (@< 0 i)))
  (check-equal? (eval-guarded! (list i- i0 i+) (list (λ-set 2) (λ-set 3) (λ-set 4)))
                (apply merge* `((,i- . 2) (,i0 . 3) (,i+ . 4))))
  (check-vc-and-cell #t #t x (apply merge* `((,i- . 2) (,i0 . 3) (,i+ . 4))))
  (check-equal? (eval-guarded! (list i- i0 i+) (list (λ-set 2) (const 3) (λ-set 4)))
                (apply merge* `((,i- . 2) (,i0 . 3) (,i+ . 4))))
  (check-vc-and-cell #t #t x (apply merge* `((,i- . 2) (,i0 . 1) (,i+ . 4))))
  (check-equal? (eval-guarded! (list i- i0 i+) (list (λ-set 2) (const 3) (const 4)))
                (apply merge* `((,i- . 2) (,i0 . 3) (,i+ . 4))))
  (check-vc-and-cell #t #t x (apply merge* `((,i- . 2) (,i0 . 1) (,i+ . 1))))
  (check-equal? (eval-guarded! (list i- i0 i+) (list (λ-set 2) (λ-set-assume 3 #f) (λ-set 4)))
                (apply merge* `((,i- . 2) (,i+ . 4))))
  (check-vc-and-cell (! (@= 0 i)) #t x (apply merge* `((,i- . 2) (,i+ . 4))))
  (check-equal? (eval-guarded! (list i- i0 i+) (list (λ-set 2) (λ-set-assert 3 #f) (λ-set 4)))
                (apply merge* `((,i- . 2) (,i+ . 4))))
  (check-vc-and-cell #t (! (@= 0 i)) x (apply merge* `((,i- . 2) (,i+ . 4))))
  (check-equal? (eval-guarded! (list i- i0 i+) (list (λ-set 2) (λ-set-err 3) (λ-set 4)))
                (apply merge* `((,i- . 2) (,i+ . 4))))
  (check-vc-and-cell #t (! (@= 0 i)) x (apply merge* `((,i- . 2) (,i+ . 4))))
  (check-equal? (eval-guarded! (list i- i0 i+) (list (λ-set-assume 2 #f) (λ-set-err 3) (λ-set 4)))
                (apply merge* `((,i+ . 4))))
  (check-vc-and-cell (! (@< i 0)) (! (@= 0 i)) x (apply merge* `((,i+ . 4))))
  (check-exn exn:fail:svm:merge?  
    (thunk (eval-guarded! (list i- i0 i+) (list (λ-set-assume 2 #f) (λ-set-err 3) (λ-set-assert 4 #f)))))
  (check-vc-and-cell (! (@< i 0)) (&& (! (@= 0 i)) (! (@< 0 i))) x 1))

(define eval-assuming-tests
  (test-suite+
   "Tests for eval-assuming in rosette/base/core/eval.rkt"
   (check-eval-assuming)))

(define eval-guarded-tests
  (test-suite+
   "Tests for eval-guarded in rosette/base/core/eval.rkt"
   (check-eval-guarded-0)
   (check-eval-guarded-1)))

(module+ test
  (time (run-tests eval-assuming-tests))
  (time (run-tests eval-guarded-tests)))