#lang racket

(provide do-trace add-original-form!
         symbolic-trace-skip-assertion?
         symbolic-trace-skip-infeasible-solver?)

(require rosette/base/core/reporter
         rosette/base/core/bool
         rosette/base/core/exn
         rosette/base/core/result
         rosette/base/core/bool
         syntax/parse/define
         (only-in "../util/syntax.rkt" syntax->readable-location)
         (only-in rosette/query/core [∃-solve query:solve])
         (only-in rosette/solver/solution unsat?))

;; set var to e, execute body, and then restore it back to e2 if exists or e otherwise
;; the result of the expression is that of body
(define-simple-macro (set+restore [var e {~optional e2}] ... #:body body ...)
  (let ([restorer (thunk (set! var {~? e2 e}) ...)])
    (set! var e) ...
    (call-with-exception-handler
     (λ (ex)
       (restorer)
       ex)
     (thunk (begin0 (let () body ...) (restorer))))))

(define symbolic-trace-skip-assertion? (make-parameter #f))
(define symbolic-trace-skip-infeasible-solver? (make-parameter #f))

(define current-entry-handler #f)

(define stats-template '([assertion . 0] [solver . 0]))

(define (do-trace proc #:entry-handler entry-handler #:post-proc post-proc)
  (set+restore
   [current-original-map (make-hash)]
   [current-trace '()]
   [current-stats (make-hash stats-template)]
   [current-entry-handler entry-handler #f]
   #:body
   (parameterize ([current-reporter reporter])
     (begin0 (proc)
       (post-proc current-stats (reverse current-trace) current-original-map)))))

;; current-syntax --------------------------------------------------------------

(define current-original-map (make-hash))

(define (add-original-form! loc-stx actual-stx)
  (define loc (syntax->readable-location loc-stx))
  (when (and (first loc) (second loc) (third loc))
    (hash-set! current-original-map loc actual-stx)))

;; trace -----------------------------------------------------------------------

(define current-stats (make-hash stats-template))

(define (collect-stats msg)
  (hash-update! current-stats msg add1))

(define current-trace '())

(define reporter
  (match-lambda*
    [(list 'exception (failed e s))
     (define assumes (vc-assumes s))
     (define asserts (vc-asserts s))
     (define skip?
       (or (exn:fail:svm:merge? e)
           ;; We skip exn:fail:svm:merge? because we already present data of
           ;; leave nodes.
           (false? assumes)
           ;; Skip false? to make it match the old behavior. We can remove this
           ;; or add an option to conditionally filter this later.
           (and (and (symbolic-trace-skip-assertion?)
                     (or (exn:fail:svm:assert:user? e)
                         (exn:fail:svm:assume:user? e)))
                (collect-stats 'assertion))
           ;; TODO: need to fix the name. symbolic-trace-skip-assertion?
           ;; doesn't make sense anymore
           (and (and (symbolic-trace-skip-infeasible-solver?)
                     (unsat? (query:solve (list assumes))))
                (collect-stats 'solver))))
     (unless skip?
       (define entry
         (list e
               (continuation-mark-set-first
                (exn-continuation-marks e)
                'symbolic-trace:stx-key)
               (continuation-mark-set->list
                (exn-continuation-marks e)
                'symbolic-trace:stack-key)
               assumes))
       (current-entry-handler
        entry
        (λ (e) (set! current-trace (cons e current-trace)))
        current-original-map))]
    [_ (void)]))
