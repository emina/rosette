#lang racket

(require rackunit/text-ui rackunit rosette/lib/roseunit "solver.rkt")
(require rosette/base/core/exn rosette/base/core/result)
(require rosette/base/adt/box)
(require (only-in rosette/base/form/define define-symbolic)
         (only-in rosette/base/core/bool
                  @boolean? @true? && ! => <=>
                  vc vc-clear! with-vc vc-merge!
                  $assume $assert @assume @assert
                  vc-true vc-true?
                  spec-assumes spec-asserts)
         (only-in rosette/base/core/real @integer? @=)
         (only-in rosette/base/core/merge merge merge*))

(provide check-vc-eqv check-exn-svm)

(define (check-vc-eqv assumes asserts)
  (define actual (vc))
  (check-unsat (solve (! (&&  (<=> (spec-assumes actual) assumes) (<=> (spec-asserts actual) asserts))))))

(define-syntax (check-exn-svm stx)
  (syntax-case stx ()
    [(_ kind? thunk) #'(check-exn-svm kind? #rx".*" thunk)]
    [(_ kind? rx thunk)
     #'(check-exn
        (lambda (ex)
          (and (kind? ex)
               (regexp-match rx (exn-message ex))))
        thunk)]))

(define-syntax-rule (check-assume-or-assert $a @a user? core? spec-a spec-other)
  (begin
    ;---------------------------;
    (define (check-vc expected-a expected-other)
      (define actual (vc))
      (check-equal? (spec-a actual) expected-a)
      (check-equal? (spec-other actual) expected-other))
    ;---------------------------;
    ($a #t)
    (check-pred vc-true? (vc))
    ($a 1)
    (check-pred vc-true? (vc))
    ;---------------------------;
    (check-exn-svm user? #rx"failed" (thunk (@a #f)))
    (check-vc #f #t)
    (vc-clear!)
    ;---------------------------;
    (check-pred vc-true? (vc))
    (check-exn-svm user? #rx"test" (thunk (@a #f "test")))
    (check-vc #f #t)
    (vc-clear!)
    ;---------------------------;
    (check-exn-svm core? #rx"failed" (thunk ($a #f)))
    (check-vc #f #t)
    (vc-clear!)
    ;---------------------------;
    (check-exn-svm core? #rx"test" (thunk ($a #f "test")))
    (check-vc #f #t)
    (vc-clear!)
    ;---------------------------;
    (define-symbolic b c @boolean?)
    (@a b)
    (check-vc b #t)
    (check-exn-svm user? #rx"contradiction" (thunk (@a (! b))))
    (check-vc  #f #t)
    (vc-clear!)
    ;---------------------------;
    ($a b)
    (check-exn-svm core? #rx"contradiction" (thunk ($a (! b))))
    (check-vc #f #t)
    (vc-clear!)
    ;---------------------------;
    ($a (merge b #f 1))
    (check-vc (@true? (merge b #f 1)) #t)
    (vc-clear!)
    ;---------------------------;
    ($a b)
    ($a c)
    (check-vc (&& b c) #t)
    (vc-clear!)))

(define (check-assumes)
   (check-assume-or-assert $assume @assume exn:fail:svm:assume:user? exn:fail:svm:assume:core?
                           spec-assumes spec-asserts))

(define (check-asserts)
   (check-assume-or-assert $assert @assert exn:fail:svm:assert:user? exn:fail:svm:assert:core?
                           spec-asserts spec-assumes))

(define (spec->pair s) (cons (spec-assumes s) (spec-asserts s)))


(define (check-with-vc-0)
  (define-symbolic a @boolean?)
  ;---------------------------;
  (check-match (with-vc (@assume 1)) (ans (? void?) (? vc-true?)))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assume a)) (ans (? void?) (app spec->pair (cons (== a) #t))))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assume #f)) (halt (? exn:fail:svm:assume:user?) (app spec->pair (cons #f #t))))
  (check-match (with-vc ($assume #f)) (halt (? exn:fail:svm:assume:core?) (app spec->pair (cons #f #t))))
  ;---------------------------;
  (check-match (with-vc (@assert 1)) (ans (? void?) (? vc-true?)))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assert a)) (ans (? void?) (app spec->pair (cons #t (== a)))))
  (check-pred vc-true? (vc))
  (check-match (with-vc (@assert #f)) (halt (? exn:fail:svm:assert:user?) (app spec->pair (cons #t #f))))
  (check-match (with-vc ($assert #f)) (halt (? exn:fail:svm:assert:core?) (app spec->pair (cons #t #f))))
  (check-match (with-vc (1)) (halt (? exn:fail:svm:assert:err?) (app spec->pair (cons #t #f)))))

(define (check-with-vc-1)
  (define-symbolic a b c d @boolean?)
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assume b))) (ans (? void?) (app spec->pair (cons (== (&& a b)) #t))))
  (check-match (with-vc (begin (@assert a) (@assert b))) (ans (? void?) (app spec->pair (cons #t (== (&& a b))))))
  (check-match (with-vc (begin (@assume a) (@assert b))) (ans (? void?) (app spec->pair (cons (== a) (== (=> a b))))))
  (check-match (with-vc (begin (@assert a) (@assume b))) (ans (? void?) (app spec->pair (cons (== (=> a b)) (== a)))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assume (! a)))) (halt (? exn:fail:svm:assume:user?) (app spec->pair (cons #f #t))))
  (check-match (with-vc (begin ($assume a) (@assume (! a)))) (halt (? exn:fail:svm:assume:user?) (app spec->pair (cons #f #t))))
  (check-match (with-vc (begin (@assume (! a)) ($assume a))) (halt (? exn:fail:svm:assume:core?) (app spec->pair (cons #f #t))))
  (check-match (with-vc (begin ($assume (! a)) ($assume a))) (halt (? exn:fail:svm:assume:core?) (app spec->pair (cons #f #t))))
  ;---------------------------;
  (check-match (with-vc (begin (@assert a) (@assert (! a)))) (halt (? exn:fail:svm:assert:user?) (app spec->pair (cons #t #f))))
  (check-match (with-vc (begin ($assert a) (@assert (! a)))) (halt (? exn:fail:svm:assert:user?) (app spec->pair (cons #t #f))))
  (check-match (with-vc (begin (@assert (! a)) ($assert a))) (halt (? exn:fail:svm:assert:core?) (app spec->pair (cons #t #f))))
  (check-match (with-vc (begin ($assert (! a)) ($assert a))) (halt (? exn:fail:svm:assert:core?) (app spec->pair (cons #t #f))))
  (check-match (with-vc (begin (@assert a) (1))) (halt (? exn:fail:svm:assert:err?) (app spec->pair (cons #t #f))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assert #f))) (halt (? exn:fail:svm:assert:user?) (app spec->pair (cons (== a) (== (! a))))))
  (check-match (with-vc (begin (@assume a) ($assert #f))) (halt (? exn:fail:svm:assert:core?) (app spec->pair (cons (== a) (== (! a))))))
  (check-match (with-vc (begin (@assume a) (1))) (halt (? exn:fail:svm:assert:err?) (app spec->pair (cons (== a) (== (! a))))))
  ;---------------------------;
  (check-match (with-vc (begin (@assert a) (@assume #f))) (halt (? exn:fail:svm:assume:user?) (app spec->pair (cons (== (! a)) (== a)))))
  (check-match (with-vc (begin (@assert a) ($assume #f))) (halt (? exn:fail:svm:assume:core?) (app spec->pair (cons (== (! a)) (== a)))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume #f) (@assert a))) (halt (? exn:fail:svm:assume:user?) (app spec->pair (cons #f #t))))
  (check-match (with-vc (begin (@assert #f) (@assume a))) (halt (? exn:fail:svm:assert:user?) (app spec->pair (cons #t #f))))
  (check-match (with-vc (begin (1) (@assume a))) (halt (? exn:fail:svm:assert:err?) (app spec->pair (cons #t #f))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (1) (@assert b))) (halt (? exn:fail:svm:assert:err?) (app spec->pair (cons (== a) (== (! a))))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assume b) (@assert c) (@assert d)))
               (ans (? void?) (app spec->pair (cons (== (&& a b)) (== (&& (=> (&& a b) c) (=> (&& a b) d)))))))
  (check-match (with-vc (begin (@assert a) (@assert b) (@assume c) (@assume d)))
               (ans (? void?) (app spec->pair (cons (== (&& (=> (&& a b) c) (=> (&& a b) d))) (== (&& a b))))))
  (check-match (with-vc (begin (@assume a) (@assert b) (@assume c) (@assert d)))
               (ans (? void?) (app spec->pair (cons (== (&& a (=> (=> a b) c))) (== (&& (=> a b) (=> (&& a (=> (=> a b) c)) d)))))))
  (check-match (with-vc (begin (@assert a) (@assume b) (@assert c) (@assume d)))
               (ans (? void?) (app spec->pair (cons (== (&& (=> a b) (=> (&& a (=> (=> a b) c)) d))) (== (&& a (=> (=> a b) c)))))))
  ;---------------------------;
  (check-match (with-vc (begin (@assume a) (@assert b) (@assume #f) (@assert d)))
               (halt (? exn:fail:svm:assume:user?) (app spec->pair (cons (== (&& a (=> (=> a b) #f))) (== (=> a b))))))
  (check-match (with-vc (begin (@assume a) (@assert b) (1) (@assert d)))
               (halt (? exn:fail:svm:assert:err?) (app spec->pair (cons (== a) (== (&& (=> a b) (=> a #f)))))))
  ;---------------------------;
  (@assume a)
  (@assert b)
  (check-match (vc) (app spec->pair (cons (== a) (== (=> a b)))))
  (check-match (with-vc (begin  (@assume c) (@assert d)))
               (ans (? void?) (app spec->pair (cons (== (&& a (=> (=> a b) c))) (== (&& (=> a b) (=> (&& a (=> (=> a b) c)) d)))))))
  (check-match (vc) (app spec->pair (cons (== a) (== (=> a b)))))
  (vc-clear!))
  
  
(define (check-vc-merge-0)
  (define-symbolic a b c @boolean?)
  ;---------------------------;
  (vc-merge! null null)
  (check-pred vc-true? (vc))
  ;---------------------------;
  (match-define (ans _ vc0) (with-vc (begin (@assume a) (@assert b))))
  (vc-merge! (list #t) (list vc0))
  (check-match (vc) (app spec->pair (cons (== a) (== (=> a b)))))
  (vc-clear!)
  (vc-merge! (list c) (list vc0))
  (check-match (vc) (app spec->pair (cons (== (=> c a)) (== (=> c (=> a b))))))
  (vc-clear!)
  (vc-merge! (list #f) (list vc0))
  (check-pred vc-true? (vc))
  ;---------------------------;
  (match-define (halt _ vc1) (with-vc (@assume #f)))
  (vc-merge! (list #f) (list vc1))
  (check-pred vc-true? (vc))
  (vc-merge! (list c) (list vc1))
  (check-match (vc) (app spec->pair (cons (== (=> c #f)) #t)))
  (check-exn-svm exn:fail:svm:assume:core? #rx"contradiction" (thunk (vc-merge! (list #t) (list vc1))))
  (vc-clear!)
  ;---------------------------;
  (match-define (halt _ vc2) (with-vc (@assert #f)))
  (vc-merge! (list #f) (list vc2))
  (check-pred vc-true? (vc))
  (vc-merge! (list c) (list vc2))
  (check-match (vc) (app spec->pair (cons #t (==  (=> c #f)))))
  (check-exn-svm exn:fail:svm:assert:core? #rx"contradiction" (thunk (vc-merge! (list #t) (list vc2))))
  (vc-clear!)
  ;---------------------------;
  (@assume a)
  (match-define (ans _ vc3) (with-vc vc-true (@assume (! a))))
  (check-exn-svm exn:fail:svm:assume:core? #rx"contradiction" (thunk (vc-merge! (list #t) (list vc3))))
  (check-match (vc) (app spec->pair (cons #f #t)))
  (vc-clear!)
  ;---------------------------;
  (@assert a)
  (match-define (ans _ vc4) (with-vc vc-true (@assert (! a))))
  (check-exn-svm exn:fail:svm:assert:core? #rx"contradiction" (thunk (vc-merge! (list #t) (list vc4))))
  (check-match (vc) (app spec->pair (cons #t #f)))
  (vc-clear!))

(define (check-vc-merge-1)
  (define-symbolic a b c d e @boolean?)
  (define not-a (! a))
  (vc-merge! (list a not-a) (list vc-true vc-true))
  (check-pred vc-true? (vc))
  ;---------------------------;
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assume c)))))
  (check-match (vc) (app spec->pair (cons (== (&& (=> a b) (=> not-a c))) #t)))
  (vc-clear!)
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assert b))) (result-state (with-vc (@assert c)))))
  (check-match (vc) (app spec->pair (cons #t (== (&& (=> a b) (=> not-a c))))))
  (vc-clear!)
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assert c)))))
  (check-match (vc) (app spec->pair (cons (== (=> a b)) (== (=> not-a c)))))
  (vc-clear!)
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assert b))) (result-state (with-vc (@assume c)))))
  (check-match (vc) (app spec->pair (cons (== (=> not-a c)) (== (=> a b)))))
  (vc-clear!)
  (@assume d)
  (@assert e)
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assert c)))))
  (check-vc-eqv (&& d (=> a (=> e b))) (&& (=> d e) (=> not-a (=> d c))))
  (vc-clear!)
  ;---------------------------;
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assume #f))) (result-state (with-vc (@assert c)))))
  (check-vc-eqv (! a) (=> not-a c))
  (vc-clear!)
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assume b))) (result-state (with-vc (@assert #f)))))
  (check-vc-eqv (=> a b) (! not-a))
  (vc-clear!)
  (check-exn-svm exn:fail:svm:assume:core? #rx"contradiction"
                 (thunk (vc-merge! (list a not-a)
                                   (list (result-state (with-vc (@assume #f)))
                                         (result-state (with-vc (@assume #f)))))))
  (check-match (vc) (app spec->pair (cons #f #t)))
  (vc-clear!)
  (check-exn-svm exn:fail:svm:assert:core? #rx"contradiction"
                 (thunk (vc-merge! (list a not-a)
                                   (list (result-state (with-vc (@assert #f)))
                                         (result-state (with-vc (@assert #f)))))))
  (check-match (vc) (app spec->pair (cons #t #f)))
  (vc-clear!)
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assume #f))) (result-state (with-vc (@assert #f)))))
  (check-match (vc) (app spec->pair (cons (== not-a) (== a))))
  (vc-merge! (list a not-a) (list (result-state (with-vc (@assert #f))) (result-state (with-vc (@assume #f)))))
  (check-match (vc) (app spec->pair (cons (== not-a) (== a))))
  (vc-clear!))

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
   "Tests for vc-merge! in rosette/base/core/vc.rkt"
   (check-vc-merge-0)
   (check-vc-merge-1)))

(module+ test
  (time (run-tests assume-tests))
  (time (run-tests assert-tests))
  (time (run-tests with-vc-tests))
  (time (run-tests vc-merge-tests)))