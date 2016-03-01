#lang rosette

(require "type.rkt" "reals.rkt"
         (only-in rosette/base/core/type type-cast)
         (only-in rosette [/ @/] [bitvector->integer bv->int] [sqrt @sqrt]))

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
         & $ ^ ~ << >>
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

; Bitwise integer operators (Ch. 6.3.a and 6.3.f).   
(define (int->bv v) (integer->bitvector v (bitvector (current-bitwidth))))

(define (& . vs) ; Bitwise and
  (bv->int (apply bvand (map int->bv vs))))

(define ($ . vs) ; Bitwise or
  (bv->int (apply bvor (map int->bv vs))))

(define (^ . vs) ; Bitwise xor
  (bv->int (apply bvxor (map int->bv vs))))

(define (~ v) ; Bitwise not
  (bv->int (bvnot (int->bv v))))

(define (<< x y) ; Left shift
  (bv->int (bvshl (int->bv x) (int->bv y))))

(define (>> x y) ; Arithmetic right shirt
  (bv->int (bvashr (int->bv x) (int->bv y))))

;; Arithmetic operators (Ch. 6.3.a): division by zero does not throw an exception.
(define (/ x y) (if (= y 0) 0 (if (equal? (real-type-of x) int) (quotient x y) (@/ x y))))
(define (% x y) (if (= y 0) 0 (remainder x y)))

(define (sqrt x) ; Bitvector approximation of square root.
  (match x
    [(or (? fixnum?) (? flonum?)) (@sqrt (max 0 x))]
    [(expression (== *) y y) (abs y)]
    [(expression (== +) (expression (== *) a a) (expression (== *) b b))
     (+ (abs a) (abs b))] ; an approximation of euclidean distance
    [_ 
     (define n (arithmetic-shift (current-bitwidth) -1))
     (define y (int->bv (real->integer (type-cast real? x 'sqrt))))
     (let loop ([res (int->bv 0)] [delta (arithmetic-shift 1 (sub1 n))] [i n])
       (if (<= i 0) 
           (bv->int res)
           (let ([tmp (bvor res (int->bv delta))])
             (loop (if (bvsge y (bvmul tmp tmp)) tmp res) 
                   (arithmetic-shift delta -1) 
                   (sub1 i)))))]))
  

;; Additional relational operators (Ch 6.3.d).
(define (!= x y) (! (= x y)))


