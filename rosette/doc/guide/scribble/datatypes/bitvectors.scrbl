#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/form rosette/query/eval rosette/solver/solution
           rosette/base/core/term 
           (only-in rosette/base/core/union union?)
           (only-in rosette/base/base bv bv? bitvector bitvector? bitvector-size
                    bveq bvslt bvsgt bvsle bvsge bvult bvugt bvule bvuge
                    bvnot bvor bvand bvxor bvshl bvlshr bvashr
                    bvneg bvadd bvsub bvmul bvudiv bvsdiv bvurem bvsrem bvsmod
                    concat extract sign-extend zero-extend 
                    integer->bitvector bitvector->integer bitvector->natural)
           (only-in rosette/base/core/safe assert) 
           (only-in rosette/base/core/bool asserts))
          (for-label racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:bitvectors"]{Bitvectors}

@declare-exporting[rosette/base/base #:use-sources (rosette/base/base)]

Rosette extends Racket with a primitive bitvector datatype whose values are
fixed-size words---or, machine integers.  Mainstream programming languages, such as
C or Java, support bitvector types with a few fixed sizes (e.g., 8 bits, 16 bits,
and 32 bits). Rosette supports bitvectors of arbitrary size, as well as both signed and
unsigned versions of various bitvector operations (such as comparisons, division, remainder, etc.).
Technically, Rosette's bitvector datatype embeds the
@hyperlink["http://smtlib.cs.uiowa.edu/logics-all.shtml#QF_BV"]{theory of bitvectors}
into a programming language.

@examples[#:eval rosette-eval
(code:line (bv 4 (bitvector 7))        (code:comment "a bitvector literal of size 7"))
(code:line (bv 4 7)                    (code:comment "a shorthand for the same literal"))
(code:line (define-symbolic x y (bitvector 7)) (code:comment "symbolic bitvector constants"))
(code:line (bvslt (bv 4 7) (bv -1 7))  (code:comment "signed 7-bit < comparison of 4 and -1"))
(code:line (bvult (bv 4 7) (bv -1 7))  (code:comment "unsigned 7-bit < comparison of 4 and -1"))
(define-symbolic b boolean?)
(code:line (bvadd x (if b y (bv 3 4))) (code:comment "this typechecks only when b is true"))
(code:line (asserts)                   (code:comment "so Rosette emits a corresponding assertion"))]

@defproc[(bitvector [size (and/c integer? positive? (not/c term?) (not/c union?))]) bitvector?]{
  Returns a type predicate that recognizes bitvectors of the given @racket[size].
  Note that @racket[size] must be a concrete positive integer.                                                        
  The type predicate itself is recognized by the @racket[bitvector?] predicate.
  @examples[#:eval rosette-eval
   (define bv6? (bitvector 6))
   (bv6? 1)
   (bv6? (bv 3 6))
   (bv6? (bv 3 5))
   (define-symbolic b boolean?)
   (bv6? (if b (bv 3 6) #t))]
}

@defproc[(bitvector? [v any/c]) boolean?]{
  Returns true if @racket[v] is a concrete type predicate that recognizes bitvector values.
   @examples[#:eval rosette-eval
   (define bv6? (bitvector 6))
   (define bv7? (bitvector 7))
   (define-symbolic b boolean?)
   (code:line (bitvector? bv6?) (code:comment "a concrete bitvector type"))
   (code:line (bitvector? (if b bv6? bv7?)) (code:comment "not a concrete type"))
   (code:line (bitvector? integer?) (code:comment "not a bitvector type"))
   (code:line (bitvector? 3) (code:comment "not a type"))]}

@defproc[(bv [val (and/c integer? (not/c term?) (not/c union?))]
             [size (and/c (or/c bitvector? (and/c integer? positive?))
                          (not/c term?) (not/c union?))]) bv?]{
  Returns a bitvector literal of the given @racket[size], which may be given either as a
  concrete @racket[bitvector?] type or a concrete positive integer.}

@defproc[(bv? [v any/c]) boolean?]{
  Recognizes concrete or symbolic bitvector values of any size.
  @examples[#:eval rosette-eval
   (bv? 1)
   (bv? (bv 1 1))
   (bv? (bv 2 2))
   (define-symbolic b boolean?)
   (bv? (if b (bv 3 6) #t))]
}

@(rosette-eval '(clear-asserts!))

@section{Comparison Operators}

@defproc*[([(bveq [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvslt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvult [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsle [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvule [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsgt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvugt [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvsge [x (bitvector n)] [y (bitvector n)]) boolean?]
           [(bvuge [x (bitvector n)] [y (bitvector n)]) boolean?])]{
Compares two bitvector values of the same bitvector type. 
Comparison relations include 
equality (@racket[bveq]) and signed / unsigned versions of
<, <=, >, and >= (@racket[bvslt], @racket[bvult], @racket[bvsle], @racket[bvule],
@racket[bvsgt], and @racket[bvugt]).
@examples[#:eval rosette-eval
(code:line (define-symbolic u v (bitvector 7)) (code:comment "symbolic bitvector constants"))
(code:line (bvslt (bv 4 7) (bv -1 7))  (code:comment "signed 7-bit < comparison of 4 and -1"))
(code:line (bvult (bv 4 7) (bv -1 7))  (code:comment "unsigned 7-bit < comparison of 4 and -1"))
(define-symbolic b boolean?)
(code:line (bvsge u (if b v (bv 3 4))) (code:comment "this typechecks only when b is true"))
(code:line (asserts)                   (code:comment "so Rosette emits a corresponding assertion"))]
}

@(rosette-eval '(clear-asserts!))

@section{Bitwise Operators}

@defproc[(bvnot [x (bitvector n)]) (bitvector n)]{
 Returns the bitwise negation of the given bitvector value.
 @examples[#:eval rosette-eval
 (bvnot (bv -1 4))
 (bvnot (bv 0 4))
 (define-symbolic b boolean?)
 (code:line (bvnot (if b 0 (bv 0 4))) (code:comment "this typechecks only when b is false"))
 (code:line (asserts)                 (code:comment "so Rosette emits a corresponding assertion"))]
}

@(rosette-eval '(clear-asserts!))

@defproc*[([(bvand [x (bitvector n)] ...+) (bitvector n)]
           [(bvor  [x (bitvector n)] ...+) (bitvector n)]
           [(bvxor [x (bitvector n)] ...+) (bitvector n)])]{
Returns the bitwise "and", "or", "xor" of one or more bitvector values of the same type.
 @examples[#:eval rosette-eval
 (bvand (bv -1 4) (bv 2 4))
 (bvor  (bv 0 3)  (bv 1 3))
 (bvxor (bv -1 5) (bv 1 5))
 (define-symbolic b boolean?)
 (code:line (bvand (bv -1 4)
                   (if b 0 (bv 2 4))) (code:comment "this typechecks only when b is false"))
 (code:line (asserts)                 (code:comment "so Rosette emits a corresponding assertion"))]
}

  
@(rosette-eval '(clear-asserts!))
@defproc*[([(bvshl  [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvlshr [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvashr [x (bitvector n)] [y (bitvector n)]) (bitvector n)])]{
Returns the left, logical right, or arithmetic right shift of @racket[x]  by
@racket[y] bits, where @racket[x] and @racket[y] are bitvector values of the same type.
 @examples[#:eval rosette-eval
 (bvshl  (bv 1 4) (bv 2 4))
 (bvlshr (bv -1 3) (bv 1 3))
 (bvashr (bv -1 5) (bv 1 5))
 (define-symbolic b boolean?)
 (code:line (bvshl (bv -1 4)
                   (if b 0 (bv 2 4))) (code:comment "this typechecks only when b is false"))
 (code:line (asserts)                 (code:comment "so Rosette emits a corresponding assertion"))]
}

@section{Arithmetic Operators}

@defproc[(bvneg [x (bitvector n)]) (bitvector n)]{
 Returns the arithmetic negation of the given bitvector value.
 @examples[#:eval rosette-eval
 (bvneg (bv -1 4))
 (bvneg (bv 0 4))
 (define-symbolic z (bitvector 3))
 (bvneg z)]
}

@(rosette-eval '(clear-asserts!))
@(rosette-eval '(clear-terms!))

@defproc*[([(bvadd [x (bitvector n)] ...+) (bitvector n)]
           [(bvsub [x (bitvector n)] ...+) (bitvector n)]
           [(bvmul [x (bitvector n)] ...+) (bitvector n)])]{
Returns the sum, difference, or product of one or more bitvector values of the same type.
 @examples[#:eval rosette-eval
 (bvadd (bv -1 4) (bv 2 4))
 (bvsub (bv 0 3)  (bv 1 3))
 (bvmul (bv -1 5) (bv 1 5))
 (define-symbolic b boolean?)
 (bvadd (bv -1 4) (bv 2 4) (if b (bv 1 4) "bad"))
 (asserts)]
}

@(rosette-eval '(clear-asserts!))

@defproc*[([(bvsdiv [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvudiv [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvsrem [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvurem [x (bitvector n)] [y (bitvector n)]) (bitvector n)]
           [(bvsmod [x (bitvector n)] [y (bitvector n)]) (bitvector n)])]{
Returns (un)signed quotient, remainder, or modulo of two bitvector values of the same type.
All five operations are defined even when the second argument is zero.
 @examples[#:eval rosette-eval
 (bvsdiv (bv -3 4) (bv 2 4))
 (bvudiv (bv -3 3) (bv 2 3))
 (bvsmod (bv 1 5) (bv 0 5))
 (define-symbolic b boolean?)
 (bvsrem (bv -3 4) (if b (bv 2 4) "bad"))
 (asserts)]
}

@(rosette-eval '(clear-asserts!))

@section{Conversion Operators}

@defproc[(concat [x bv?] ...+) bv?]{
 Returns the bitwise concatenation of the given bitvector values.
 @examples[#:eval rosette-eval
 (concat (bv -1 4) (bv 0 1) (bv -1 3))
 (define-symbolic b boolean?)
 (concat (bv -1 4) (if b (bv 0 1) (bv 0 2)) (bv -1 3))]
}

@defproc[(extract [i integer?] [j integer?] [x (bitvector n)]) (bitvector (+ 1 (- i j)))]{
 Extracts bits @racket[i] down to @racket[j] from a bitvector of size @racket[n], yielding a
 bitvector of size i - j + 1.  This procedure assumes that @racket[n] > @racket[i] >= @racket[j] >= 0.
 @examples[#:eval rosette-eval
 (extract 2 1 (bv -1 4))
 (extract 3 3 (bv 1 4))
 (define-symbolic i j integer?)
 (eval:alts (extract i j (bv 1 2)) (begin (extract i j (bv 1 2)) "{[(&& (= 0 j) (= 1 i)) (bv 1 2)] ...}"))
 (asserts)]
}

@(rosette-eval '(clear-asserts!))

@defproc*[([(sign-extend [x bv?] [t (or/c bitvector? union?)]) bv?]
           [(zero-extend [x bv?] [t (or/c bitvector? union?)]) bv?])]{
Returns a bitvector of type @racket[t] that represents the (un)signed
extension of the bitvector @racket[x].
Note that both @racket[x] and @racket[t] may be symbolic. The size of @racket[t]
must not be smaller than the size of @racket[x]'s type.
 @examples[#:eval rosette-eval
 (sign-extend (bv -3 4) (bitvector 6))
 (zero-extend (bv -3 4) (bitvector 6))
 (define-symbolic b c boolean?)
 (zero-extend (bv -3 4) (if b (bitvector 5) (bitvector 6)))
 (zero-extend (bv -3 4) (if b (bitvector 5) "bad"))
 (asserts)
 (zero-extend (bv -3 4) (if c (bitvector 5) (bitvector 1)))
 (asserts)]
}

@(rosette-eval '(clear-asserts!))

@defproc*[([(bitvector->integer [x bv?]) integer?]
           [(bitvector->natural [x bv?]) integer?])]{
Returns the (un)signed integer value of the given bitvector.
 @examples[#:eval rosette-eval
 (bitvector->integer (bv -1 4))
 (bitvector->natural (bv -1 4))
 (define-symbolic b boolean?)
 (bitvector->integer (if b (bv -1 3) (bv -3 4)))
 (bitvector->integer (if b (bv -1 3) "bad"))
 (asserts)]
}

@(rosette-eval '(clear-asserts!))

@defproc*[([(integer->bitvector [i integer?] [t (or/c bitvector? union?)]) bv?])]{
Returns a bitvector of type @racket[t] that represents the @var[k] lowest order bits
of the integer @racket[i], where @var[k] is the size of @racket[t].              
Note that both @racket[i] and @racket[t] may be symbolic.  
 @examples[#:eval rosette-eval
 (integer->bitvector 4 (bitvector 2))
 (integer->bitvector 15 (bitvector 4))
 (define-symbolic b c boolean?)
 (integer->bitvector (if b pi 3) (if c (bitvector 5) (bitvector 6)))
 (asserts)]
}

@(kill-evaluator rosette-eval)
