#lang racket

(require
  (only-in "bool.rkt" with-vc $assume merge-vc!)
   "exn.rkt" "result.rkt" "store.rkt" "merge.rkt")

(provide eval-assuming eval-guarded!)

; Takes as input a concrete or symbolic boolean and a thunk, 
; evaluates thunk under the assumption that the guard holds,
; and returns the result. This result takes one of two forms.
;
; If the evaluation of the thunk terminates normally, the result
; is (normal (normal v st) vc*) where v is the value computed by the
; thunk, st captures all stores mutations performed during evaluation,
; and vc* captures the verification condition generated during the
; evaluation, starting from the current vc.
;
; If the thunk terminates abnormally, the result is (failed ex vc*),
; where ex is an exn:fail:svm? exception that represents the cause
; of the abnormal termination, and vc* captures the verification
; condition generated during the evaluation, starting from the current vc.
;
; Neither the current store nor the current vc are modified after
; eval-assuming returns.
(define (eval-assuming guard thunk)
  (with-vc
      (begin 
        ($assume guard)
        (with-store (thunk)))))

; Takes as input a list of n guards and n thunks, evaluates each thunk
; under its guard using eval-assuming, merges the resulting vcs into
; the current vc, merges the resulting stores (if any) into the current
; store, and merges the resulting values (if any) before returning them
; as output. If all of the thunks fail under their guards, eval-guarded
; raises an exn:fail:svm:merge exception after the specs are merged into
; the current vc.
; This procedure makes the following assumptions, based on the Lean
; formalization:
; (1) At most one guard evaluates to true under any model.
; (2) For all models m under which (vc) evaluates to vc-true, there is
; exactly one guard in guards that evaluates to #t under m.
; (3) For all models m under which (vc) doesn't evaluate to vc-true,
; every vc produced by evaluating the given thunks evaluates to
; the same spec as (vc) under m.
(define (eval-guarded! guards thunks)
  (define results (map eval-assuming guards thunks))
  (merge-vc! guards (map result-state results))
  (define-values (gs rs)
    (for/lists (gs rs) ([g guards][r results] #:when (normal? r))
      (values g (result-value r))))
  (if (null? rs)
      (raise-exn:fail:svm:merge)
      (begin
        (merge-stores! gs (map result-state rs))
        (apply merge* (for/list ([g gs][r rs])
                        (cons g (result-value r)))))))


    