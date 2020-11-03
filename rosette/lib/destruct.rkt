#lang racket

(require syntax/parse/define
         (only-in rosette for/all for*/all)
         (for-syntax racket/struct-info racket/list))

(provide destruct-lambda destruct destruct*)

(begin-for-syntax
  (define-syntax-class sub-pattern
    #:description "a sub-pattern"
    (pattern {~or* {~datum _} {~datum ...} {~datum ___}}
             #:attr id-set '())
    (pattern x:id
             #:when (regexp-match #px"^(\\.\\.|__)\\d+$" (symbol->string (syntax-e #'x)))
             #:attr id-set '())
    (pattern x:id
             #:attr id-set (list #'x)))

  (define-syntax-class head-pattern
    #:description "a head pattern"
    (pattern {~or* {~datum list}
                   {~datum list-rest}
                   {~datum list*}
                   ;; list-no-order is generally buggy, so let's not include it.
                   {~datum vector}
                   {~datum cons}
                   {~datum box}})
    ;; structs
    (pattern x:id #:when (struct-info? (syntax-local-value #'x (λ () #f)))))

  (define-syntax-class clause-pattern
    (pattern (head:head-pattern expr:sub-pattern ...)
             #:attr id-set (append* (attribute expr.id-set)))
    (pattern x:sub-pattern
             #:attr id-set (attribute x.id-set)))

  (define (check-duplicate-identifier! xs)
    (define dup-result (check-duplicate-identifier xs))
    (when dup-result
      (raise-syntax-error #f "duplicate binding identifier" dup-result))))

(define-simple-macro (destruct-lambda [pat expr ...+] ...)
  (lambda (v) (destruct v [pat expr ...] ...)))

;; NOTE: we need to wrap the RHS of a clause in `begin` to disallow
;; the special meaning of => in [_ (=> fail) (fail)]

(define-simple-macro (destruct val [pat:clause-pattern e:expr ...+] ...)
  #:do [(for-each check-duplicate-identifier! (attribute pat.id-set))]
  #:with result
  (syntax/loc this-syntax
    (for/all ([var val]);(guarded-values val)])
      (match var [pat (begin e ...)] ...)))
  result)

(define-simple-macro (destruct* (val ...) [(pat:clause-pattern ...) e:expr ...+] ...)
  #:do [(for-each (λ (xs) (check-duplicate-identifier! (append* xs)))
                  (attribute pat.id-set))]
  #:with (var ...) (generate-temporaries #'(val ...))
  #:with result
  (syntax/loc this-syntax
    (for*/all ([var val] ...); (guarded-values val)] ...)
      (match* (var ...) [(pat ...) (begin e ...)] ...)))
  result)
