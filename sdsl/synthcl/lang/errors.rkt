#lang rosette

(provide raise-arity-error raise-operator-arity-error 
         raise-no-common-type-error raise-bad-type-error
         raise-bad-form-error) 

(define (raise-arity-error what expected expr [subexpr #f])
  (raise-syntax-error #f (format "wrong number of ~a (expected ~a)" what expected) expr subexpr))

(define (raise-operator-arity-error expected expr [subexpr #f])
  (raise-arity-error "operands" expected expr subexpr))

(define (raise-no-common-type-error expr [kind "real"])
  (raise-syntax-error #f (format "no common ~a type for operands" kind) expr))

(define (raise-bad-type-error expected actual expr [subexpr #f])
  (raise-syntax-error 
   #f
   (format "expected ~a, given ~a" expected actual)
   expr subexpr))

(define (raise-bad-form-error expected expr [subexpr #f])
  (raise-syntax-error #f (format "expected ~a" expected) expr subexpr))
