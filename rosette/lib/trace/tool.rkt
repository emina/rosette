#lang racket

(provide do-trace record-apply! add-current-syntax! restore-current-syntax!
         add-original-form!
         symbolic-trace-skip-assertion?
         symbolic-trace-skip-infeasible-solver?)

(require rosette/base/core/reporter
         rosette/base/core/bool
         rosette/base/core/exn
         racket/exn
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
   [current-syntax-list '()]
   [current-activated-syntax #f]
   [current-original-map (make-hash)]
   [current-trace '()]
   [current-stats (make-hash stats-template)]
   [current-stack '()]
   [current-activated-stack #f]
   [current-entry-handler entry-handler #f]
   #:body
   (parameterize ([current-reporter reporter])
     (begin0 (proc)
       (post-proc current-stats (reverse current-trace) current-original-map)))))

;; current-syntax --------------------------------------------------------------

(define current-syntax-list '())
(define current-activated-syntax #f)

(define (add-current-syntax! stx)
  (set! current-syntax-list (cons stx current-syntax-list)))

(define (restore-current-syntax! [e #f])
  (when (and e (not current-activated-syntax))
    (set! current-activated-syntax (first current-syntax-list)))
  (set! current-syntax-list (rest current-syntax-list))
  e)

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
    [(list 'exception guard e)
     (define the-pc (&& (pc) guard))
     (define skip?
       (or (exn:fail:rosette:infeasible? e)
           (and (and (symbolic-trace-skip-assertion?)
                     (exn:fail:rosette:assertion? e))
                (collect-stats 'assertion))
           (and (and (symbolic-trace-skip-infeasible-solver?)
                     (unsat? (query:solve (list the-pc))))
                (collect-stats 'solver))))
     (unless skip?
       (define entry (list e
                           (or current-activated-syntax (first current-syntax-list))
                           (or current-activated-stack current-stack)
                           the-pc))
       (current-entry-handler entry
                              (λ (e) (set! current-trace (cons e current-trace)))
                              current-original-map))
     (set! current-activated-syntax #f)
     (set! current-activated-stack #f)]
    [_ (void)]))


;; stack -----------------------------------------------------------------------

(define current-stack '())
(define current-activated-stack #f)

(define (runner proc*)
  (lambda (loc proc _in)
    (set! current-stack (cons (cons proc loc) current-stack))
    (call-with-exception-handler
     (lambda (e)
       (unless current-activated-stack
         (set! current-activated-stack current-stack))
       (set! current-stack (rest current-stack))
       e)
     (thunk (begin0 (proc*) (set! current-stack (rest current-stack)))))))

(define record-apply!
  (make-keyword-procedure
   (lambda (kws kw-args loc proc . rest)
     ((runner (thunk (keyword-apply proc kws kw-args rest)))
      loc proc (append rest kw-args)))
   (lambda (loc proc . rest)
     ((runner (thunk (apply proc rest))) loc proc rest))))
