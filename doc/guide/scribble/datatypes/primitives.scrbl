#lang scribble/manual

@(require (for-label 
           rosette/base/define rosette/query/tools rosette/query/eval rosette/solver/solution
           rosette/base/term (only-in rosette/base/num current-bitwidth) 
           (only-in rosette/base/safe assert) 
           (only-in rosette/base/assert asserts)
           (only-in rosette/base/enum enum?)
           (only-in rosette/base/base << >> >>>))
          (for-label racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@(define bools (select '(boolean? not false? true false boolean=? nand nor implies xor)))

@(define nums (select '(number? complex? real? rational? integer? exact-integer? exact-nonnegative-integer? exact-positive-integer? inexact-real? fixnum? flonum? double-flonum? single-flonum? zero? positive? negative? even? odd? exact? inexact? inexact->exact exact->inexact real->single-flonum real->double-flonum + - * / quotient remainder quotient/ modulo add1 sub1 abs max min gcd lcm round floor ceiling truncate numerator denominator rationalize = < <= > >= sqrt integer-sqrt integer-sqrt/ expt exp log sin cos tan asin acos atan make-rectangular make-polar real-part imag-part magnitude angle bitwise-ior bitwise-and bitwise-xor bitwise-not bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length random random-seed make-pseudo-random-generator pseudo-random-generator? current-pseudo-random-generator pseudo-random-generator->vector vector->pseudo-random-generator vector->pseudo-random-generator! pseudo-random-generator-vector? number->string string->number real->decimal-string integer-bytes->integer integer->integer-bytes floating-point-bytes->real real->floating-point-bytes system-big-endian? pi pi.f degrees->radians radians->degrees sqr sgn conjugate sinh cosh tanh exact-round exact-floor exact-ceiling exact-truncate order-of-magnitude nan? infinite?)))


@title[#:tag "sec:primitives"]{Booleans and Numbers}

@declare-exporting[rosette/base/base #:use-sources (rosette/base/num rosette/base/base)]

Rosette divides built-in datatypes into two kinds: @deftech[#:key "primitive datatype"]{primitive} and 
@deftech[#:key "composite datatype"]{composite}. Both kinds of 
datatypes include concrete Racket values and symbolic Rosette values, but only primitive 
datatypes include symbolic constants, introduced by @seclink["sec:symbolic-constants-and-assertions"]{@code{define-symbolic[*]}}. 
The boolean and number types are the sole primitive datatypes in Rosette.  Values of these types are recognized 
using the @racket[boolean?] and @racket[number?] predicates. 


Rosette lifts the following operations on primitive datatypes, including a few additional operations on 
numbers (@defidentifier[#'>>], @defidentifier[#'>>>], @defidentifier[#'<<]) that have their usual meaning from C or Java:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Booleans} @bools)
      (list @elem{Numbers}  @elem{@nums, @racket[>>], @racket[>>>], @racket[<<]}))]


Lifted boolean operations retain their Racket semantics on both concrete and symbolic values. 
In particular, Rosette extends the intepretation of these operations to work on symbolic values in (logically) the 
same way that they work on concrete values.

@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(boolean? b)
(boolean? #t)
(boolean? #f)
(boolean? 1)
(code:line (not b) (code:comment "produces a logical negation of b"))]

Lifted numeric operations, in contrast, only match their Racket semantics when applied to concrete values.  
Symbolic numbers are treated as signed finite precision integers, and all operations 
that involve symbolic numbers employ finite (rather than arbitrary) precision computations. 
Applying an operation to a concrete and a symbolic number implicitly coerces the concrete 
number to a finite integer representation.  

@examples[#:eval rosette-eval
(+ 4.584294 pi) 
(define-symbolic n number?)
(code:line (define sol (solve (assert (= n pi))))  (code:comment "pi is coerced to 3,"))
(code:line (evaluate n sol)                        (code:comment "so n is bound to 3"))]

@defparam[current-bitwidth bitwidth (and/c integer? positive?)
          #:value 5]{
  The @racket[current-bitwidth] 
  parameter controls the precision of numeric operations on symbolic values, by specifying the number of bits in
  the signed representation of 
  a symbolic number. Default is 5 bits.  This parameter should be kept as 
  small as possible to ensure faster evaluation of @seclink["sec:queries"]{solver-aided queries}.
  As a general rule, it should also be set once, before any numeric operations are evaluated. 
  @examples[#:eval rosette-eval
                   (code:line (current-bitwidth 4) (code:comment "use 4-bit precision for symbolic operations"))
                   (define sol          
                     (solve (begin (assert (> n 0))
                                   (assert (< (add1 n) 0)))))
                   (code:line (evaluate n sol)     (code:comment "7 + 1 = -8 in 4-bit signed representation"))                                          
]
}

@(kill-evaluator rosette-eval)