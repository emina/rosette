#lang racket

(require syntax/parse/define
         (only-in rosette for/all for*/all)
         (for-syntax racket/struct-info racket/list))

(provide destruct-lambda destruct destruct*)

(begin-for-syntax
  (define-syntax-class sub-pattern
    #:description "a sub-pattern"
    (pattern {~and x {~or* {~datum _} {~datum ...} {~datum ___}}}
             #:with parsed #'x
             #:attr id-set '())
    (pattern x:id
             #:when (regexp-match #px"^(\\.\\.|__)\\d+$" (symbol->string (syntax-e #'x)))
             #:with parsed #'x
             #:attr id-set '())
    (pattern x:id
             #:with parsed #'x
             #:attr id-set (list #'x)))

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

  (define-syntax-class parse-clause-pattern
    (pattern (head:head-pattern expr:sub-pattern ...)
             #:with parsed (syntax/loc this-syntax (head expr.parsed ...))
             #:attr id-set (append* (attribute expr.id-set)))
    (pattern x:sub-pattern
             #:with parsed #'x.parsed
             #:attr id-set (attribute x.id-set)))

  (define-syntax-class clause-pattern
    (pattern x:parse-clause-pattern
             #:with parsed #'x.parsed
             #:do [(define dup-result (check-duplicate-identifier
                                       (attribute x.id-set)))
                   (when dup-result
                     (raise-syntax-error #f "duplicate binding identifier"
                                         dup-result))])))

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
