#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/form rosette/query/eval rosette/solver/solution
           rosette/base/core/term (only-in rosette/query/finitize current-bitwidth)
           (only-in rosette/base/base ! && || => <=> exists forall function?)
           (only-in rosette/base/core/safe assert) 
           (only-in rosette/base/core/bool asserts))
          (except-in (for-label racket) =>)
          scribble/core scribble/html-properties scribble/eval racket/sandbox racket/runtime-path
          "../util/lifted.rkt")


@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "bools-log")))

@(define bools (select '(boolean? false? true false boolean=? not nand nor implies xor)))

@(define nums (select '(number? complex? real? rational? integer? exact-integer? exact-nonnegative-integer? exact-positive-integer? inexact-real? fixnum? flonum? double-flonum? single-flonum? zero? positive? negative? even? odd? exact? inexact? inexact->exact exact->inexact real->single-flonum real->double-flonum + - * / quotient remainder quotient/ modulo add1 sub1 abs max min gcd lcm round floor ceiling truncate numerator denominator rationalize = < <= > >= sqrt integer-sqrt integer-sqrt/ expt exp log sin cos tan asin acos atan make-rectangular make-polar real-part imag-part magnitude angle bitwise-ior bitwise-and bitwise-xor bitwise-not bitwise-bit-set? bitwise-bit-field arithmetic-shift integer-length random random-seed make-pseudo-random-generator pseudo-random-generator? current-pseudo-random-generator pseudo-random-generator->vector vector->pseudo-random-generator vector->pseudo-random-generator! pseudo-random-generator-vector? number->string string->number real->decimal-string integer-bytes->integer integer->integer-bytes floating-point-bytes->real real->floating-point-bytes system-big-endian? pi pi.f degrees->radians radians->degrees sqr sgn conjugate sinh cosh tanh exact-round exact-floor exact-ceiling exact-truncate order-of-magnitude nan? infinite?)))


@title[#:tag "sec:bools+ints+reals"]{Booleans, Integers, and Reals}

@declare-exporting[rosette/base/base  #:use-sources (rosette/base/base)]

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

@section[#:tag "sec:extra-bools"]{Additional Logical Operators}

In addition to lifting Racket's operations on booleans, Rosette also supports the following
logical operations:  conjunction (@racket[&&]), disjunction (@racket[||]), implication (@racket[=>]),
bi-implication (@racket[<=>]), negation (@racket[!]), universal quantification (@racket[forall]), and
existential quantification (@racket[exists]). These operations have their usual logical meaning
(e.g., unlike Racket's shortcircuiting @racket[and] operator, the logical @racket[&&] operator
evaluates all of its arguments before taking their conjunction). 

@(rosette-eval '(clear-asserts!))

@defproc[(! [v boolean?]) boolean?]{
  Returns the negation of the given boolean value.
  @examples[#:eval rosette-eval
   (! #f)
   (! #t)
   (define-symbolic b boolean?)
   (code:line (! (if b #f 3)) (code:comment "this typechecks only when b is true"))
   (code:line (asserts)       (code:comment "so Rosette emits a corresponding assertion"))]
}

@(rosette-eval '(clear-asserts!))

@defproc*[([(&& [v boolean?] ...) boolean?]
           [(|| [v boolean?] ...) boolean?])]{
Returns the logical conjunction or disjunciton of zero or more boolean values.
 @examples[#:eval rosette-eval
 (&&)
 (||)
 (code:line (&& #f (begin (displayln "hello") #t)) (code:comment "no shortcircuiting"))
 (define-symbolic a b boolean?)
 (code:line (&& a (if b #t 1)) (code:comment "this typechecks only when b is true"))
 (code:line (asserts)          (code:comment "so Rosette emits a corresponding assertion"))]
}

@(rosette-eval '(clear-asserts!))

@defproc*[([(=>  [x boolean?] [y boolean?]) boolean?]
           [(<=> [x boolean?] [y boolean?]) boolean?])]{
Returns the logical implication or bi-implication of two boolean values.
 @examples[#:eval rosette-eval
 (code:line (=> #f (begin (displayln "hello") #f)) (code:comment "no shortcircuiting"))
 (define-symbolic a b boolean?)
 (code:line (<=> a (if b #t 1)) (code:comment "this typechecks only when b is true"))
 (code:line (asserts)           (code:comment "so Rosette emits a corresponding assertion"))]
}

@(rosette-eval '(clear-asserts!))
@defproc*[([(forall [vs (listof constant?)] [body boolean?]) boolean?]
           [(exists [vs (listof constant?)] [body boolean?]) boolean?])]{
Returns a universally or existentially @deftech{quantified formula}, where the
symbolic constants @racket[vs] are treated as quantified variables.  The @racket[body]
of the formula is a boolean expression over the quantified variables @racket[vs] and,
optionally, over free symbolic (Skolem) constants.

If a set of constraints is satisfiable, their @racket[model] includes bindings only for
free symbolic constants:  no bindings are provided for constants that do not appear freely in any formula.
All quantified symbolics must be have a non-@racket[function?] @racket[solvable?] type.
The usual lexical scoping rules apply to quantified symbolics; if @racket[body] is
a quantified formula over a variable @var[v] in @racket[vs], then the
innermost quantification of @var[v] shadows any enclosing quantifications.

When executing queries over assertions that contain quantified formulas,
the @racket[current-bitwidth] parameter must be set to @racket[#f].
Quantified formulas may not appear in any assertion that is passed to a @racket[synthesize] query,
either via an (implicit or explicit) assumption or a guarantee expression.

@examples[#:eval rosette-eval
 (current-bitwidth #f)
 (define-symbolic a b integer?)
 (forall (list) (= a b))
 (code:line (define f (forall (list a) (exists (list b) (= a (+ a b))))) (code:comment "no free constants"))
 (code:line (solve (assert f)) (code:comment "so the model has no bindings"))
 (code:line (define g (forall (list a) (= a (+ a b)))) (code:comment "b is free in g")) 
 (code:line (solve (assert g)) (code:comment "so the model has a binding for b"))
 (code:line (define h (exists (list a) (forall (list a) (= a (+ a a))))) (code:comment "body refers to the innermost a"))
 (code:line (solve (assert h)) (code:comment "so h is unsatisfiable."))
]



}

@(kill-evaluator rosette-eval)