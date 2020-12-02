#lang racket

(require (only-in racket/string string-split)
         (for-syntax racket/syntax racket/string)
         racket/provide)

; Four kinds of failures can happen during symbolic evaluation:
; (1) the execution reaches (assert e) where e evaluates to #f, or asserting e reduces vc's asserts to #f; 
; (2) the execution reaches (assume e) where e evaluates to #f, or assuming e reduces vc's assumes to #f; 
; (3) the execution reaches e where e raises an exn:fail? exception; and 
; (4) all paths at a given merge point led to a failure.
; Within the first two types of failures, we distinguish between
; assertions and assumptions issued by user code and core (Rosette) code.
; The third type of failure is treated as an assertion failure for the
; purposes of verification condition generation. Finally,
; the fourth type of failure is tracked via exn:fail:svm:merge.

; The top of the exception hierarchy for failures raised
; during symbolic evaluation.
(struct exn:fail:svm exn:fail ())

; An assert exception can be one of the following kinds:
; * :core represents an assertion failure raised in Rosette code,
; * :user represents an assertion failure raised in user code, and 
; * :err  indicates that an exn:fail? exception was raised during evaluation. 
(struct exn:fail:svm:assert exn:fail:svm ())
(struct exn:fail:svm:assert:core exn:fail:svm:assert ())
(struct exn:fail:svm:assert:user exn:fail:svm:assert ())
(struct exn:fail:svm:assert:err exn:fail:svm:assert ())

; An assume exception can be one of the following kinds:
; * :core represents an assumption failure raised in Rosette code, and 
; * :user represents an assumption failure raised in user code.
(struct exn:fail:svm:assume exn:fail:svm ())
(struct exn:fail:svm:assume:core exn:fail:svm:assume ())
(struct exn:fail:svm:assume:user exn:fail:svm:assume ())

; An merge exception is raised when all paths at a branching point lead to a failure.
(struct exn:fail:svm:merge exn:fail:svm ())

(define-syntax (define-make-and-raise stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([make-id (format-id #'id "make-~a" (syntax-e #'id))]
                   [raise-id (format-id #'id "raise-~a" (syntax-e #'id))]
                   [prefix (list-ref (string-split (symbol->string (syntax-e #'id)) ":") 3)])
       #'(begin
           (define (make-id [msg #f] [cont-marks #f])
             (id  (format "~a: ~a" prefix (or msg "failed"))
                  (or cont-marks (current-continuation-marks))))
           (define (raise-id [msg #f] [cont-marks #f])
             (raise (make-id msg cont-marks)))))]
    [(_ id ...)
     #'(begin (define-make-and-raise id) ...)]))

; Creates two procedures make-* and raise-* for each exception type that
; creates and raises an exception of the given type, respectively.
(define-make-and-raise
  exn:fail:svm:assert:core
  exn:fail:svm:assert:user
  exn:fail:svm:assert:err
  exn:fail:svm:assume:core
  exn:fail:svm:assume:user
  exn:fail:svm:merge)
  
(provide (matching-identifiers-out #px"^exn:fail:svm.*\\?$" (all-defined-out))
         (matching-identifiers-out #px"^make\\-exn:fail:svm.*$" (all-defined-out))
         (matching-identifiers-out #px"^raise\\-exn:fail:svm.*$" (all-defined-out)))


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