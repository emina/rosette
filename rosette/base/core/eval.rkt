#lang racket

(require
  (only-in "bool.rkt" pc)
  "vc.rkt" "exn.rkt" "result.rkt" "store.rkt" "merge.rkt")

(provide eval-assuming eval-guarded)

; Takes as input a concrete or symbolic boolean and a thunk, 
; evaluates thunk under the assumption that the guard holds,
; and returns the result. This result takes one of two forms.
;
; If the evaluation of the thunk terminates normally, the result
; is (ans (ans v st) sp) where v is the value computed by the
; thunk, st captures all stores mutations performed during evaluation,
; and sp captures the verification condition generated during the
; evaluation, starting from the current vc.
;
; If the thunk terminates abnormally, the result is (halt ex sp),
; where ex is an exn:fail:svm? exception that represents the cause
; of the abnormal termination, and s is the specification
; (i.e., assumes and asserts) generated during the evaluation, starting
; from the current vc.
;
; Neither the current store nor the current vc are modified after
; eval-assuming returns.
(define (eval-assuming guard thunk)
  (with-vc
      (begin 
        ($assume guard)
        (with-store (thunk)))))

; Takes as input a list of n guards and n thunks, evaluates each thunk
; under its guard using eval-assuming, merges the resulting specs into
; the current vc, merges the resulting stores (if any) into the current
; store, and merges the resulting values (if any) before returning them
; as output. If all of the thunks fail under their guards, eval-guarded
; raises an exn:fail:svm:merge exception after the specs are merged into
; the current vc.
(define (eval-guarded guards thunks)
  (define results (map eval-assuming guards thunks))
  (merge-specs! guards (map result-state results))
  (define-values (gs rs)
    (for/lists (gs rs) ([g guards][r results] #:when (ans? r))
      (values g (result-value r))))
  (if (null? rs)
      (raise-exn:fail:svm:merge)
      (begin
        (merge-stores! gs (map result-state rs))
        (apply merge* (for/list ([g gs][r rs])
                        (cons g (result-value r)))))))

; ----------------------------------------------------------- ;

(provide speculate)

(define (return-false e) #f)

; Temporary code to integrate and test new store tracking
; module, before implementing VC collection and handling.
; 
; This form extends the PC with the given guard, calls
; (with-store body) under that guard and returns the result,
; if with-store executes normally under the guard. Otherwise,
; this form catches any exn:fail? exception thrown by with-store
; and returns #f. This mimics the behavior old speculate* form.
(define-syntax-rule (speculate guard body)
  (with-handlers ([exn:fail? return-false])
    (with-store (parameterize ([pc guard]) body))))
    