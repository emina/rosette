#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/form rosette/query/eval rosette/solver/solution
           rosette/base/core/term (only-in rosette/query/finitize current-bitwidth) 
           (only-in rosette/base/core/safe assert) 
           (only-in rosette/base/core/bool asserts))
          (for-label racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@(define bools (select '(boolean? false? true false boolean=? not nand nor implies xor)))

@(define nums (select '(number? complex? real? rational? integer? exact-integer? exact-nonnegative-integer? exact-positive-integer? inexact-real? fixnum? flonum? double-flonum? single-flonum? zero? positive? negative? even? odd? exact? inexact? inexact->exact exact->inexact real->single-flonum real->double-flonum + - * / quotient remainder quotient/ modulo add1 sub1 abs max min gcd lcm round floor ceiling truncate numerator denominator rationalize = < <= > >= sqrt integer-sqrt integer-sqrt/ expt exp log sin cos tan asin acos atan make-rectangular make-polar real-part imag-part magnitude angle bitwise-ior bitwise-and bitwise-xor bitwise-not bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length random random-seed make-pseudo-random-generator pseudo-random-generator? current-pseudo-random-generator pseudo-random-generator->vector vector->pseudo-random-generator vector->pseudo-random-generator! pseudo-random-generator-vector? number->string string->number real->decimal-string integer-bytes->integer integer->integer-bytes floating-point-bytes->real real->floating-point-bytes system-big-endian? pi pi.f degrees->radians radians->degrees sqr sgn conjugate sinh cosh tanh exact-round exact-floor exact-ceiling exact-truncate order-of-magnitude nan? infinite?)))


@title[#:tag "sec:bools+ints+reals"]{Booleans, Integers, and Reals}

@declare-exporting[rosette/base/base #:use-sources (rosette/base/base)]

Rosette lifts the following operations on booleans, integers, and reals:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Booleans} @bools)
      (list @elem{Integers and Reals}  @nums))]


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

Lifted numeric operations, in contrast, match their Racket semantics
only when applied to concrete values.  Their symbolic semantics depends on the
current @tech["reasoning precision"], as determined by the @racket[current-bitwidth]
parameter.  In particular, if this parameter is set to @racket[#f], operations on symbolic numbers
retain their infinite-precision Racket semantics.  However, because infinite-precision
reasoning is not efficiently (or at all) decidable for arbitrary numeric operations,
@racket[current-bitwidth] is, by default, set to a small positive integer @var[k].
With this setting, symbolic numbers are treated as signed @var[k]-bit integers. See
@secref{sec:reasoning-precision} for details and examples.


@(kill-evaluator rosette-eval)