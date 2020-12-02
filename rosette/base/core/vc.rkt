#lang racket

(require
  (only-in "term.rkt" term? term-type)
  (only-in "bool.rkt" @boolean? @true? ! && =>)
  "exn.rkt" "result.rkt")

(provide @assert @assume $assert $assume
         (rename-out [vc-ref vc]) vc-clear! merge-specs! with-vc
         spec-assumes spec-asserts
         spec-tt spec-tt?)

; A spec consists of two (symbolic or concrete) @boolean?
; values representing the assumptions and assertions issued
; so far. A spec is legal if at least one of its assumes or
; asserts is true under all models.
(struct spec (assumes asserts) #:transparent)

; The true (top) spec.
(define spec-tt (spec #t #t))

(define (spec-tt? s) (equal? s spec-tt))

; A conjunction of specs s1 ... sn is a spec that conjoins their
; assumes and asserts elementwise, i.e., 
; (spec (s1.assumes && ... && sn.assumes) (s1.asserts && ... && sn.asserts))
(define spec-and
  (case-lambda
    [()  spec-tt]
    [(s) s]
    [(s1 s2)  (spec (&& (spec-assumes s1) (spec-assumes s2))
                    (&& (spec-asserts s1) (spec-asserts s2)))]
    [ss (spec (apply && (map spec-assumes ss))
              (apply && (map spec-asserts ss)))]))

; Guarding a spec s with a guard g, which must be a concrete or
; symbolic boolean, produces the spec (spec (g => s.assumes) (g => s.asserts)).
(define (spec-guard s g)
  (spec (=> g (spec-assumes s)) (=> g (spec-asserts s))))

; Returns (spec (s.assumes && (s.asserts => g) s.asserts). 
(define (assuming s g)  ; g must be a symbolic or concrete boolean
  (spec (&& (spec-assumes s) (=> (spec-asserts s) g)) (spec-asserts s)))

; Returns (spec s.assumes (s.asserts && (s.assumes => g))). 
(define (asserting s g) ; g must be a symbolic or concrete boolean 
  (spec (spec-assumes s) (&& (spec-asserts s) (=> (spec-assumes s) g))))

; The vc parameter keeps track of the current verification condition,
; which is an instance of spec?. The default vc is the true spec.
(define vc (make-parameter
            spec-tt
            (lambda (v) (unless (spec? v) (raise-argument-error 'vc "spec?" v)) v)))

; Returns the current vc, without exposing the parameter outside the module. 
(define (vc-ref) (vc))

; Clears the current vc by setting to the true spec.
(define (vc-clear!) (vc spec-tt))

; Takes as input a list of n guards and n specs and sets the current vc
; to (vc) && (spec-guard guard1 specs1) && ... && (spec-guard guardn specn).
; Then, it checks if either the assumes or the asserts of the resulting spec
; are false? and if so, throws either an exn:fail:svm:assume? or
; exn:fail:svm:assert? exception.
(define (merge-specs! guards specs)
  (unless (null? specs)
    (define specm (apply spec-and (vc) (map spec-guard specs guards)))
    (vc specm)
    (when (false? (spec-assumes specm))
      (raise-exn:fail:svm:assume:core "contradiction"))
    (when (false? (spec-asserts specm))
      (raise-exn:fail:svm:assert:core "contradiction"))))

(define (vc-set! val msg spec-proc spec-field raise-exn)
  (let* ([guard (@true? val)]
         [specg (spec-proc (vc) guard)])
    (vc specg)
    (when (false? guard)
      (raise-exn (msg)))
    (when (false? (spec-field specg))
      (raise-exn "contradiction"))))

; Sets the current vc to (asserting (vc) g) where g is (@true? val).
; If g is #f, throws an exn:fail:svm:assert exception of the given 
; kind. If the resulting vc's asserts field is #f, throws an
; exn:fail:svm:assert exception of kind 'eval.
(define (vc-assert! val msg raise-kind)
  (vc-set! val msg asserting spec-asserts raise-kind))

; Sets the current vc to (assuming (vc) g) where g is (@true? val).
; If g is #f, throws an exn:fail:svm:assume exception of the given
; kind. If the resulting vc's assumes field is #f, throws an
; exn:fail:svm:assume exception of kind 'eval.
(define (vc-assume! val msg raise-kind)
  (vc-set! val msg assuming spec-assumes raise-kind))

; The $assert form has three variants: ($assert val), ($assert val msg),
; and ($assert val msg kind), where val is the value being asserted, msg
; is the failure message, and kind is a procedure that returns a subtype of
; exn:fail:svm:assert. Default values for msg and kind are #f and
; raise-exn:fail:svm:assert:core, respectively.
; The first two variants of this form are used for issuing assertions from
; within the Rosette core. The third variant is used to implement the @assert
; form that is exposed to user code. An $assert call modifies the current vc to
; reflect the issued assertion. If the issued assertion or the spec-assert of the
; current vc reduce to #f, the call throws an exception of the given kind after
; updating the vc.
(define-syntax ($assert stx)
  (syntax-case stx ()
    [(_ val)          (syntax/loc stx ($assert val #f  raise-exn:fail:svm:assert:core))]
    [(_ val msg)      (syntax/loc stx ($assert val msg raise-exn:fail:svm:assert:core))]
    [(_ val msg kind) (syntax/loc stx (vc-assert! val (thunk msg) kind))]))

; Analogous to the $assert form, except that it modifies the current vc to
; reflect the issued assumption.
(define-syntax ($assume stx)
  (syntax-case stx ()
    [(_ val)          (syntax/loc stx ($assume val #f   raise-exn:fail:svm:assume:core))]
    [(_ val msg)      (syntax/loc stx ($assume val msg  raise-exn:fail:svm:assume:core))]
    [(_ val msg kind) (syntax/loc stx (vc-assume! val (thunk msg) kind))]))

; The @assert form modifies the current vc to reflect the issued assertion.
; The form has two variants (@assert val) and (@assert val msg), where val
; is the value being asserted and msg is the optional error message in case
; val is #f. This form is exposed to user code.
(define-syntax (@assert stx)
  (syntax-case stx ()
    [(_ val)     (syntax/loc stx ($assert val #f  raise-exn:fail:svm:assert:user))]
    [(_ val msg) (syntax/loc stx ($assert val msg raise-exn:fail:svm:assert:user))]))

; The @assume form modifies the current vc to reflect the issued assumption.
; The form has two variants (@assume val) and (@assume val msg), where val
; is the value being assume and msg is the optional error message in case
; val is #f. This form is exposed to user code.
(define-syntax (@assume stx)
  (syntax-case stx ()
    [(_ val)     (syntax/loc stx ($assume val #f  raise-exn:fail:svm:assume:user))]
    [(_ val msg) (syntax/loc stx ($assume val msg raise-exn:fail:svm:assume:user))]))

(define (halt-svm ex)
  (halt ex (vc)))

(define (halt-err ex) ; Treat an exn:fail? error as an assertion failure.
  (halt (make-exn:fail:svm:assert:err (exn-message ex) (exn-continuation-marks ex))
        (asserting (vc) #f)))

; The with-vc form has two variants, (with-vc body) and (with-vc vc0 body).
; The former expands into (with-vc (vc) body). The latter sets the current
; vc to vc0, evaluates the given body, returns the result, and reverts vc
; to the value it held before the call to with-vc.
;
; If the evaluation of the body terminates normally, (with-vc vc0 body)
; outputs (ans v s) where v is the value computed by the body, and s is 
; the specification (i.e., assumes and asserts) generated during the evaluation,
; with v0 as the initial specification. 
;
; If the evaluation of the body terminates abnormally with an exn:fail? exception,
; (with-vc vc0 body) outputs (halt v s) where v is an exn:fail:svm? exception
; that represents the cause of the abnormal termination, and s is the specification
; (i.e., assumes and asserts) generated during the evaluation, with v0 as
; the initial specification.
(define-syntax (with-vc stx)
  (syntax-case stx ()
    [(_ body) (syntax/loc stx (with-vc (vc) body))]
    [(_ vc0 body)
     (syntax/loc stx
       (parameterize ([vc vc0])
         (with-handlers ([exn:fail:svm? halt-svm]
                         [exn:fail?     halt-err])
           (ans body (vc)))))]))
