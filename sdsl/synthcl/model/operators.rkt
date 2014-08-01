#lang s-exp rosette

(require "type.rkt" "reals.rkt" (only-in rosette [/ rosette//]))

; This module provides the implementation for a subset of 
; arithmetic operators for each OpenCL scalar type.  These
; operators are lifted to work on vector value in the front
; end, after typechecking, using the apply-operator, 
; apply-selector and apply-comparison.  See "../lang/operators.rkt" for 
; details. 

(provide apply-selector apply-operator apply-comparison
         ; Scalar ternary selection operator (Ch. 6.3.i)
         (rename-out [if ?:])
         ; Real and integer operators (Ch. 6.3.a)
         + - * / % sqrt abs
         (rename-out [expt pow])
         ; Bitwise integer operators (Ch. 6.3.a and 6.3.f).   
         (rename-out [bitwise-and &] [bitwise-ior $]
                     [bitwise-xor ^] [bitwise-not ~]
                     [>> >>] [<< <<])
         ; Comparison operators (Ch 6.3.d).
         (rename-out [= ==]) != < <= > >=
         ; Logic operators (Ch 6.3.g).
         (rename-out [and &&] [or ||] [! !]))

; Produces the result of applying the ternary selection operator 
; ? to three values, a, b and c that have all been coerced to the 
; given common vector type. See Ch. 6.3.i.
(define (apply-selector crt a b c)
  (let ([base (type-base crt)])
    (apply crt
           (for/list ([idx (real-type-length crt)])
             (if (< (vector-ref a idx) 0)
                 ((base) (vector-ref b idx))
                 ((base) (vector-ref c idx)))))))

; Returns the result of applying the given scalar real or integer 
; operator to one or more operands that have all been coerced to the given 
; common vector type.
(define (apply-operator crt op . args)
  (define len (real-type-length crt))
  (define base (type-base crt))
  (apply 
   crt
   (for/list ([idx len])
     ((base) (apply op (for/list ([v args]) 
                         (vector-ref v idx)))))))

; Returns the result of applying the given scalar comparison operator to 
; one or more operands that have all been coerced to a common vector type.
; The result of the operation is the given integer type.
(define (apply-comparison intk op x y)
  (apply intk
         (for/list ([idx (real-type-length intk)])
           (if (op (vector-ref x idx) (vector-ref y idx)) 
               -1 
               0))))

;; Arithmetic operators (Ch. 6.3.a): division by zero does not throw an exception.
(define (/ x y) (if (= y 0) 0 (if (equal? (real-type-of x) int) (quotient x y) (rosette// x y))))
(define (% x y) (if (= y 0) 0 (remainder x y)))

;; Additional relational operators (Ch 6.3.d).
(define (!= x y) (! (= x y)))


