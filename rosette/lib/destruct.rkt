#lang racket

(require syntax/parse/define
         (only-in rosette for/all for*/all)
         (for-syntax racket/struct-info))

(provide destruct-lambda destruct destruct*)

(begin-for-syntax
  (define-syntax-class sub-pattern
    #:description "a sub-pattern"
    ;; this includes ... and _
    (pattern x:id #:with parsed #'x))

  (define-syntax-class head-pattern
    #:description "a head pattern"
    (pattern {~datum list})
    (pattern {~datum list-rest})
    (pattern {~datum list*})
    ;; list-no-order is generally buggy, so let's not include it.
    (pattern {~datum vector})
    (pattern {~datum cons})
    (pattern {~datum box})
    ;; structs
    (pattern x:id #:when (struct-info? (syntax-local-value #'x (λ () #f)))))

  (define-syntax-class clause-pattern
    (pattern (head:head-pattern expr:sub-pattern ...)
             #:with parsed (syntax/loc this-syntax (head expr.parsed ...)))
    (pattern x:sub-pattern #:with parsed #'x.parsed)))

(define-simple-macro (destruct-lambda [pat expr ...+] ...)
  (lambda (v) (destruct v [pat expr ...] ...)))

;; NOTE: we need to wrap the RHS of a clause in `begin` to disallow
;; the special meaning of => in [_ (=> fail) (fail)]

(define-simple-macro (destruct val [pat:clause-pattern e:expr ...+] ...)
  #:with result
  (syntax/loc this-syntax
    (for/all ([var val]);(guarded-values val)])
      (match var [pat.parsed (begin e ...)] ...)))
  result)

(define-simple-macro (destruct* (val ...) [(pat:clause-pattern ...) e:expr ...+] ...)
  #:with (var ...) (generate-temporaries #'(val ...))
  #:with result
  (syntax/loc this-syntax
    (for*/all ([var val] ...); (guarded-values val)] ...)
      (match* (var ...) [(pat.parsed ...) (begin e ...)] ...)))
  result)
