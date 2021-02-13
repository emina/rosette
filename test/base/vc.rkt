#lang racket

(require rackunit/text-ui rackunit rosette/lib/roseunit "solver.rkt")
(require rosette/base/core/exn rosette/base/core/result)
(require rosette/base/adt/box)
(require (only-in rosette/base/form/define define-symbolic)
         (only-in rosette/base/core/bool
                  @boolean? @true? && ! => <=>
                  vc clear-vc! with-vc merge-vc!
                  $assume $assert @assume @assert
                  vc-true vc-true?
                  vc-assumes vc-asserts)
         (only-in rosette/base/core/real @integer? @=)
         (only-in rosette/base/core/merge merge merge*))

(provide check-vc-eqv check-exn-svm)

(define (check-vc-eqv assumes asserts)
  (define actual (vc))
  (check-unsat (solve (! (&&  (<=> (vc-assumes actual) assumes) (<=> (vc-asserts actual) asserts))))))

(define-syntax (check-exn-svm stx)
  (syntax-case stx ()
    [(_ kind? thunk) #'(check-exn-svm kind? #rx".*" thunk)]
    [(_ kind? rx thunk)
     #'(check-exn
        (lambda (ex)
          (and (kind? ex)
               (regexp-match rx (exn-message ex))))
        thunk)]))

(define-syntax-rule (check-assume-or-assert $a @a user? core? vc-a vc-other)
  (begin
    ;---------------------------;
    (define (check-vc expected-a expected-other)
      (define actual (vc))
      (check-equal? (vc-a actual) expected-a)
      (check-equal? (vc-other actual) expected-other))
    ;---------------------------;
    ($a #t)
    (check-pred vc-true? (vc))
    ($a 1)
    (check-pred vc-true? (vc))
    ;---------------------------;
    (check-exn-svm user? #rx"failed" (thunk (@a #f)))
    (check-vc #f #t)
    (clear-vc!)
    ;---------------------------;
    (check-pred vc-true? (vc))
    (check-exn-svm user? #rx"test" (thunk (@a #f "test")))
    (check-vc #f #t)
    (clear-vc!)
    ;---------------------------;
    (check-exn-svm core? #rx"failed" (thunk ($a #f)))
    (check-vc #f #t)
    (clear-vc!)
    ;---------------------------;
    (check-exn-svm core? #rx"test" (thunk ($a #f "test")))
    (check-vc #f #t)
    (clear-vc!)
    ;---------------------------;
    (define-symbolic b c @boolean?)
    (@a b)
    (check-vc b #t)
    (check-exn-svm user? #rx"contradiction" (thunk (@a (! b))))
    (check-vc  #f #t)
    (clear-vc!)
    ;---------------------------;
    ($a b)
    (check-exn-svm core? #rx"contradiction" (thunk ($a (! b))))
    (check-vc #f #t)
    (clear-vc!)
    ;---------------------------;
    ($a (merge b #f 1))
    (check-vc (@true? (merge b #f 1)) #t)
    (clear-vc!)
    ;---------------------------;
    ($a b)
    ($a c)
    (check-vc (&& b c) #t)
    (clear-vc!)))

(define (check-assumes)
   (check-assume-or-assert $assume @assume exn:fail:svm:assume:user? exn:fail:svm:assume:core?
                           vc-assumes vc-asserts))

(define (check-asserts)
   (check-assume-or-assert $assert @assert exn:fail:svm:assert:user? exn:fail:svm:assert:core?
                           vc-asserts vc-assumes))

(define (vc->pair s) (cons (vc-assumes s) (vc-asserts s)))


(define (check-with-vc-0)
  (define-symbolic a @boolean?)
  ;---------------------------;
  (check-match (with-vc (@assume 1)) (normal (? void?) (? vc-true?)))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assume a)) (normal (? void?) (app vc->pair (cons (== a) #t))))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assume #f)) (failed (? exn:fail:svm:assume:user?) (app vc->pair (cons #f #t))))
  (check-match (with-vc ($assume #f)) (failed (? exn:fail:svm:assume:core?) (app vc->pair (cons #f #t))))
  ;---------------------------;
  (check-match (with-vc (@assert 1)) (normal (? void?) (? vc-true?)))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assert a)) (normal (? void?) (app vc->pair (cons #t (== a)))))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assert #f)) (failed (? exn:fail:svm:assert:user?) (app vc->pair (cons #t #f))))
  (check-match (with-vc ($assert #f)) (failed (? exn:fail:svm:assert:core?) (app vc->pair (cons #t #f))))
  (check-match (with-vc (1)) (failed (? exn:fail:svm:assert:err?) (app vc->pair (cons #t #f)))))

(define (check-with-vc-1)
  (define-symbolic a b c d @boolean?)
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assume b))) (normal (? void?) (app vc->pair (cons (== (&& a b)) #t))))
  (check-match (with-vc (begin (@assert a) (@assert b))) (normal (? void?) (app vc->pair (cons #t (== (&& a b))))))
  (check-match (with-vc (begin (@assume a) (@assert b))) (normal (? void?) (app vc->pair (cons (== a) (== (=> a b))))))
  (check-match (with-vc (begin (@assert a) (@assume b))) (normal (? void?) (app vc->pair (cons (== (=> a b)) (== a)))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assume (! a)))) (failed (? exn:fail:svm:assume:user?) (app vc->pair (cons #f #t))))
  (check-match (with-vc (begin ($assume a) (@assume (! a)))) (failed (? exn:fail:svm:assume:user?) (app vc->pair (cons #f #t))))
  (check-match (with-vc (begin (@assume (! a)) ($assume a))) (failed (? exn:fail:svm:assume:core?) (app vc->pair (cons #f #t))))
  (check-match (with-vc (begin ($assume (! a)) ($assume a))) (failed (? exn:fail:svm:assume:core?) (app vc->pair (cons #f #t))))
  ;---------------------------;
  (check-match (with-vc (begin (@assert a) (@assert (! a)))) (failed (? exn:fail:svm:assert:user?) (app vc->pair (cons #t #f))))
  (check-match (with-vc (begin ($assert a) (@assert (! a)))) (failed (? exn:fail:svm:assert:user?) (app vc->pair (cons #t #f))))
  (check-match (with-vc (begin (@assert (! a)) ($assert a))) (failed (? exn:fail:svm:assert:core?) (app vc->pair (cons #t #f))))
  (check-match (with-vc (begin ($assert (! a)) ($assert a))) (failed (? exn:fail:svm:assert:core?) (app vc->pair (cons #t #f))))
  (check-match (with-vc (begin (@assert a) (1))) (failed (? exn:fail:svm:assert:err?) (app vc->pair (cons #t #f))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assert #f))) (failed (? exn:fail:svm:assert:user?) (app vc->pair (cons (== a) (== (! a))))))
  (check-match (with-vc (begin (@assume a) ($assert #f))) (failed (? exn:fail:svm:assert:core?) (app vc->pair (cons (== a) (== (! a))))))
  (check-match (with-vc (begin (@assume a) (1))) (failed (? exn:fail:svm:assert:err?) (app vc->pair (cons (== a) (== (! a))))))
  ;---------------------------;
  (check-match (with-vc (begin (@assert a) (@assume #f))) (failed (? exn:fail:svm:assume:user?) (app vc->pair (cons (== (! a)) (== a)))))
  (check-match (with-vc (begin (@assert a) ($assume #f))) (failed (? exn:fail:svm:assume:core?) (app vc->pair (cons (== (! a)) (== a)))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume #f) (@assert a))) (failed (? exn:fail:svm:assume:user?) (app vc->pair (cons #f #t))))
  (check-match (with-vc (begin (@assert #f) (@assume a))) (failed (? exn:fail:svm:assert:user?) (app vc->pair (cons #t #f))))
  (check-match (with-vc (begin (1) (@assume a))) (failed (? exn:fail:svm:assert:err?) (app vc->pair (cons #t #f))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (1) (@assert b))) (failed (? exn:fail:svm:assert:err?) (app vc->pair (cons (== a) (== (! a))))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assume b) (@assert c) (@assert d)))
               (normal (? void?) (app vc->pair (cons (== (&& a b)) (== (&& (=> (&& a b) c) (=> (&& a b) d)))))))
  (check-match (with-vc (begin (@assert a) (@assert b) (@assume c) (@assume d)))
               (normal (? void?) (app vc->pair (cons (== (&& (=> (&& a b) c) (=> (&& a b) d))) (== (&& a b))))))
  (check-match (with-vc (begin (@assume a) (@assert b) (@assume c) (@assert d)))
               (normal (? void?) (app vc->pair (cons (== (&& a (=> (=> a b) c))) (== (&& (=> a b) (=> (&& a (=> (=> a b) c)) d)))))))
  (check-match (with-vc (begin (@assert a) (@assume b) (@assert c) (@assume d)))
               (normal (? void?) (app vc->pair (cons (== (&& (=> a b) (=> (&& a (=> (=> a b) c)) d))) (== (&& a (=> (=> a b) c)))))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assert b) (@assume #f) (@assert d)))
               (failed (? exn:fail:svm:assume:user?) (app vc->pair (cons (== (&& a (=> (=> a b) #f))) (== (=> a b))))))
  (check-match (with-vc (begin (@assume a) (@assert b) (1) (@assert d)))
               (failed (? exn:fail:svm:assert:err?) (app vc->pair (cons (== a) (== (&& (=> a b) (=> a #f)))))))
  ;---------------------------;
  (@assume a)
  (@assert b)
  (check-match (vc) (app vc->pair (cons (== a) (== (=> a b)))))
  (check-match (with-vc (begin  (@assume c) (@assert d)))
               (normal (? void?) (app vc->pair (cons (== (&& a (=> (=> a b) c))) (== (&& (=> a b) (=> (&& a (=> (=> a b) c)) d)))))))
  (check-match (vc) (app vc->pair (cons (== a) (== (=> a b)))))
  (clear-vc!))
  
  
(define (check-vc-merge-0)
  (define-symbolic a b c @boolean?)
  ;---------------------------;
  (merge-vc! null null)
  (check-pred vc-true? (vc))
  ;---------------------------;
  (match-define (normal _ vc0) (with-vc (begin (@assume a) (@assert b))))
  (merge-vc! (list #t) (list vc0))
  (check-match (vc) (app vc->pair (cons (== a) (== (=> a b)))))
  (clear-vc!)
  (merge-vc! (list c) (list vc0))
  (check-match (vc) (app vc->pair (cons (== (=> c a)) (== (=> c (=> a b))))))
  (clear-vc!)
  (merge-vc! (list #f) (list vc0))
  (check-pred vc-true? (vc))
  ;---------------------------;
  (match-define (failed _ vc1) (with-vc (@assume #f)))
  (merge-vc! (list #f) (list vc1))
  (check-pred vc-true? (vc))
  (merge-vc! (list c) (list vc1))
  (check-match (vc) (app vc->pair (cons (== (=> c #f)) #t)))
  (check-exn-svm exn:fail:svm:assume:core? #rx"contradiction" (thunk (merge-vc! (list #t) (list vc1))))
  (clear-vc!)
  ;---------------------------;
  (match-define (failed _ vc2) (with-vc (@assert #f)))
  (merge-vc! (list #f) (list vc2))
  (check-pred vc-true? (vc))
  (merge-vc! (list c) (list vc2))
  (check-match (vc) (app vc->pair (cons #t (==  (=> c #f)))))
  (check-exn-svm exn:fail:svm:assert:core? #rx"contradiction" (thunk (merge-vc! (list #t) (list vc2))))
  (clear-vc!)
  ;---------------------------;
  (@assume a)
  (match-define (normal _ vc3) (with-vc vc-true (@assume (! a))))
  (check-exn-svm exn:fail:svm:assume:core? #rx"contradiction" (thunk (merge-vc! (list #t) (list vc3))))
  (check-match (vc) (app vc->pair (cons #f #t)))
  (clear-vc!)
  ;---------------------------;
  (@assert a)
  (match-define (normal _ vc4) (with-vc vc-true (@assert (! a))))
  (check-exn-svm exn:fail:svm:assert:core? #rx"contradiction" (thunk (merge-vc! (list #t) (list vc4))))
  (check-match (vc) (app vc->pair (cons #t #f)))
  (clear-vc!))

(define (check-vc-merge-1)
  (define-symbolic a b c d e @boolean?)
  (define not-a (! a))
  (merge-vc! (list a not-a) (list vc-true vc-true))
  (check-pred vc-true? (vc))
  ;---------------------------;
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assume c)))))
  (check-match (vc) (app vc->pair (cons (== (&& (=> a b) (=> not-a c))) #t)))
  (clear-vc!)
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assert b))) (result-state (with-vc (@assert c)))))
  (check-match (vc) (app vc->pair (cons #t (== (&& (=> a b) (=> not-a c))))))
  (clear-vc!)
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assert c)))))
  (check-match (vc) (app vc->pair (cons (== (=> a b)) (== (=> not-a c)))))
  (clear-vc!)
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assert b))) (result-state (with-vc (@assume c)))))
  (check-match (vc) (app vc->pair (cons (== (=> not-a c)) (== (=> a b)))))
  (clear-vc!)
  (@assume d)
  (@assert e)
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assert c)))))
  (check-vc-eqv (&& d (=> a (=> e b))) (&& (=> d e) (=> not-a (=> d c))))
  (clear-vc!)
  ;---------------------------;
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assume #f))) (result-state (with-vc (@assert c)))))
  (check-vc-eqv (! a) (=> not-a c))
  (clear-vc!)
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assert #f)))))
  (check-vc-eqv (=> a b) (! not-a))
  (clear-vc!)
  (check-exn-svm exn:fail:svm:assume:core? #rx"contradiction"
                 (thunk (merge-vc! (list a not-a)
                                   (list (result-state (with-vc (@assume #f)))
                                         (result-state (with-vc (@assume #f)))))))
  (check-match (vc) (app vc->pair (cons #f #t)))
  (clear-vc!)
  (check-exn-svm exn:fail:svm:assert:core? #rx"contradiction"
                 (thunk (merge-vc! (list a not-a)
                                   (list (result-state (with-vc (@assert #f)))
                                         (result-state (with-vc (@assert #f)))))))
  (check-match (vc) (app vc->pair (cons #t #f)))
  (clear-vc!)
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assume #f))) (result-state (with-vc (@assert #f)))))
  (check-match (vc) (app vc->pair (cons (== not-a) (== a))))
  (merge-vc! (list a not-a) (list (result-state (with-vc (@assert #f))) (result-state (with-vc (@assume #f)))))
  (check-match (vc) (app vc->pair (cons (== not-a) (== a))))
  (clear-vc!))

(define assume-tests
  (test-suite+
   "Basic assume tests for rosette/base/core/vc.rkt"
   (check-assumes)))

(define assert-tests
  (test-suite+
   "Basic assert tests for rosette/base/core/vc.rkt"
   (check-asserts)))

(define with-vc-tests
  (test-suite+
   "Tests for with-vc in rosette/base/core/vc.rkt"
   (check-with-vc-0)
   (check-with-vc-1)))

(define vc-merge-tests
  (test-suite+
   "Tests for merge-vc! in rosette/base/core/vc.rkt"
   (check-vc-merge-0)
   (check-vc-merge-1)))

(module+ test
  (time (run-tests assume-tests))
  (time (run-tests assert-tests))
  (time (run-tests with-vc-tests))
  (time (run-tests vc-merge-tests)))