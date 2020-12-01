#lang racket/base

; Five kinds of failures can happen during symbolic evaluation:
; (1) the execution reaches (assert e) where e evaluates to #f,
; (2) the execution reaches (assume e) where e evaluates to #f,
; (3) the execution reaches e where e raises an exn:fail? exception;
; (4) partial evaluation reduces either the assert or assume component of (vc) to #f, and
; (5) all paths at a given merge point led to a failure.
; Within the first two types of failures, we distinguish between
; assertions and assumptions issued by user code and core (Rosette) code.
; The third type of failure is treated as an assertion failure for the
; purposes of verification condition generation. The fourth type of
; failure is treated as either an assertion or assumption failure, depending on which
; vc component is reduced to #f. We maintain the invariant that at most of one
; of these components can ever evaluate to #f under any model. Finally,
; the fifth type of failure is tracked via exn:fail:svm:merge.

; The top of the exception hierarchy for failures raised
; during symbolic evaluation.
(struct exn:fail:svm exn:fail ())

; An assert exception can be one of the following kinds:
; * `core represents an assertion failure raised in Rosette code,
; * `user represents an assertion failure raised in user code,
; * `eval indicates that the assertion component of a (vc) was reduced to #f through partial evaluation, and 
; * `err  indicates that an exn:fail? exception was raised during evaluation. 
(struct exn:fail:svm:assert exn:fail:svm (kind))

; An assume exception can be one of the following kinds:
; * core represents an assumption failure raised in Rosette code,
; * user represents an assumption failure raised in user code, and 
; * eval indicates that the assumption component of a (vc) was reduced to #f through partial evaluation.
(struct exn:fail:svm:assume exn:fail:svm (kind))

; An merge exception is raised when all paths at a branching point lead to a failure.
(struct exn:fail:svm:merge exn:fail:svm ())

; Returns an exception of type exn:fail:svm:assert with the given kind, optional message, and
; optional continuation marks. The kind argument must be one of the following symbols:
; `core, `user, `eval, `err. 
(define (make-assert-exn kind [msg #f] [cont-marks #f])
  (case kind
    [(core user eval err)
     (exn:fail:svm:assert (format "assert: ~a" (or msg "failed"))
                          (or cont-marks (current-continuation-marks)) kind)]
    [else
     (raise-argument-error 'raise-assert-error "one of `(core user eval err)" kind)]))

; Raises an exception of type exn:fail:svm:assert with the given kind, optional message, and
; optional continuation marks. The kind argument must be one of the following symbols:
; `core, `user, `eval, `err. 
(define (raise-assert-exn kind [msg #f] [cont-marks #f])
  (raise (make-assert-exn kind msg cont-marks)))

; Returns an exception of type exn:fail:svm:assume with the given kind, optional message, and
; optional continuation marks. The kind argument must be one of the following symbols:
; `core, `user, `eval.
(define (make-assume-exn kind [msg #f] [cont-marks #f])
  (case kind
    [(core user eval)
     (exn:fail:svm:assume (format "assume: ~a" (or msg "failed"))
                                 (or cont-marks (current-continuation-marks)) kind)]
    [else
     (raise-argument-error 'raise-assume-error "one of `(core user eval)" kind)]))

; Raises an exception of type exn:fail:svm:assume with the given kind, optional message, and
; optional continuation marks. The kind argument must be one of the following symbols:
; `core, `user, `eval.
(define (raise-assume-exn kind [msg #f] [cont-marks #f])
  (raise (make-assume-exn kind msg cont-marks)))

; Makes an exception of type exn:fail:svm:merge with a default message and
; continuation marks.  
(define (make-merge-exn)
  (exn:fail:svm:merge "merge: all paths failed" (current-continuation-marks)))

; Raises an exception of type exn:fail:svm:merge with a default message and
; continuation marks.  
(define (raise-merge-exn)
  (raise (make-merge-exn)))
  

(provide exn:fail:svm? exn:fail:svm:assert? exn:fail:svm:assume? exn:fail:svm:merge?
         exn:fail:svm:assert-kind exn:fail:svm:assume-kind
         make-assert-exn make-assume-exn make-merge-exn
         raise-assert-exn raise-assume-exn raise-merge-exn)


(provide raise-exn:fail:rosette:infeasible
         raise-exn:fail:rosette:assertion
         raise-exn:fail:rosette:assumption
         exn:fail:rosette?
         exn:fail:rosette:infeasible?
         exn:fail:rosette:assertion?
         exn:fail:rosette:assumption?)

(struct exn:fail:rosette exn:fail ())
(struct exn:fail:rosette:infeasible exn:fail:rosette ())
(struct exn:fail:rosette:assertion exn:fail:rosette ())
(struct exn:fail:rosette:assumption exn:fail:rosette ())

(define (raise-exn:fail:rosette:infeasible)
  (raise (exn:fail:rosette:infeasible
          "pc: infeasible path condition"
          (current-continuation-marks))))

(define (raise-exn:fail:rosette:assertion msg)
  (raise (exn:fail:rosette:assertion
          (string-append "assert: " msg)
          (current-continuation-marks))))

(define (raise-exn:fail:rosette:assumption msg)
  (raise (exn:fail:rosette:assertion
          (string-append "assume: " msg)
          (current-continuation-marks))))