#lang scribble/manual

@(require (for-label 
           rosette/solver/solver rosette/solver/solution rosette/query/query
           rosette/base/form/define rosette/query/eval (only-in rosette/base/base bitvector ~>)
           rosette/base/core/term rosette/base/core/type rosette/base/core/union
           (only-in rosette/base/core/bool asserts)
           (only-in rosette/base/core/reflect symbolics)
           rosette/base/core/forall rosette/lib/lift ;rosette/lib/match 
           (only-in rosette/base/core/safe assert) 
           racket)
           ;(except-in racket match match* match-lambda))
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:value-reflection"]{Reflecting on Symbolic Values}

There are two kinds of symbolic values in Rosette:  symbolic terms and 
symbolic unions.  A Rosette program can inspect the representation of 
both kinds of values.  This is useful for @tech[#:key "lifted constructs"]{lifting} additional 
(unlifted) Racket procedures to work on symbolic values, and for 
controlling the performance of Rosette's symbolic evaluator.

@section[#:tag "sec:symbolic-terms"]{Symbolic Terms}

@declare-exporting[rosette/base/core/term rosette/base/core/reflect #:use-sources (rosette/base/core/type rosette/base/core/term rosette/base/core/reflect)]

A @deftech{symbolic term} is either a symbolic constant, created via
@seclink["sec:symbolic-constants"]{@code{define-symbolic[*]}}, 
or a symbolic expression, produced by a lifted operator.
Terms are strongly typed, and always belong to a @tech{solvable type}.
Symbolic values of all other (@tech[#:key "unsolvable type"]{unsolvable}) types take the form of 
@seclink["sec:symbolic-unions"]{symbolic unions}.

@deftogether[(@defproc[(term? [v any/c]) boolean?]
              @defproc[(expression? [v any/c]) boolean?]
              @defproc[(constant? [v any/c]) boolean?])]{
Predicates for recognizing symbolic terms, expressions, and constants, respectively.
@examples[#:eval rosette-eval
(code:line (define-symbolic x integer?) (code:comment "constant"))
(code:line (define e (+ x 1)) (code:comment "expression"))
(list (term? x) (term? e))
(list (constant? x) (constant? e))
(list (expression? x) (expression? e))
(term? 1)]
}

@(rosette-eval '(require racket/match))
@deftogether[(@defform[(term content type)]
              @defform[(expression op child ...+)]
              @defform[(constant id type)])]{
Pattern matching forms for symbolic terms, expressions, and constants, respectively.
@examples[#:eval rosette-eval
(code:line (define-symbolic x integer?) (code:comment "constant"))
(code:line (define e (+ x 1)) (code:comment "expression"))
(match x
  [(constant identifier type) (list identifier type)])
(match x
  [(term content type) (list content type)])
(match e
  [(expression op child ...) (cons op child)])
(match e
  [(term content type) (list content type)])]}

@defproc[(symbolics [v any/c]) (listof constant?)]{
Returns all symbolic constants that are
transitively contained in the given value.
@examples[#:eval rosette-eval
(define-symbolic x y z integer?) 
(symbolics x)
(symbolics (if (= x y) 2 #f))
(symbolics (list y z y))
(struct cell (value) #:transparent)
(symbolics (list 5 (cons (box y) z) (vector (cell x) z)))]
}

@defproc*[([(type-of [v any/c] ...+) type?])]{
Returns the most specific @racket[type?] predicate that accepts all of the given values.
@examples[#:eval rosette-eval
(define-symbolic x integer?)
(type-of x) 
(type-of (+ x 1))
(type-of x 3.14)
(type-of #t)
(type-of #t 1)]
}

@defproc[(type? [v any/c]) boolean?]{
Returns true when given a predicate that recognizes a @seclink["ch:built-in-datatypes"]{built-in} or
a @seclink["ch:programmer-defined-datatypes"]{structure} type.  Otherwise returns false.
@examples[#:eval rosette-eval
(type? integer?)
(type? boolean?)
(type? list?)
(struct circle (radius))
(type? circle?)
(type? any/c)
(type? 1)]
}

@defproc[(solvable? [v any/c]) boolean?]{
Returns true if @racket[v] is a type predicate for a @tech{solvable type}.                                        
@examples[#:eval rosette-eval
(solvable? boolean?)
(solvable? integer?)
(solvable? real?)
(solvable? (~> (bitvector 3) (bitvector 4)))
(solvable? list?)
(struct circle (radius))
(solvable? circle?)
(solvable? any/c)
]}

@section[#:tag "sec:symbolic-unions"]{Symbolic Unions}

@declare-exporting[rosette/base/core/union #:use-sources (rosette/base/core/union)]

Rosette represents symbolic values of an @tech[#:key "unsolvable type"]{unsolvable type}
(e.g., @racket[list?]) as @deftech[#:key "symbolic union"]{symbolic unions}.
A symbolic union is a set of two or more @deftech[#:key "guarded value"]{guarded values}.
A guarded value, in turn, combines a guard, which is a symbolic @racket[boolean?] term,
and a (non-union) value.  Rosette's symbolic evaluator guarantees that the guards in
a symbolic union are disjoint:  only one of them can ever be true.   For example, the
symbolic vector @racket[s] defined below is represented as a symbolic union of two guarded vectors:
@interaction[#:eval rosette-eval
(define-symbolic b boolean?)
(define v (vector 1))
(define w (vector 2 3))
(define s (if b v w))
s
(type-of s)
(eq? s v)
(eq? s w)]

The values that appear in a union are themselves never unions.  They may, however, contain unions.
They may also belong to several different types.  In that case, the type of the union is the most
specific @racket[type?] predicate that accepts all members of the union.
This will always be an unsolvable type---possibly @racket[any/c], the most general unsolvable type.
@interaction[#:eval rosette-eval
(define-symbolic b boolean?)
(define-symbolic c boolean?)
(define v (if c "c" 0))
(define u (if b (vector v) 4))
u
(type-of u)]

Symbolic unions are recognized by the @racket[union?] predicate, and Rosette programs can inspect
union contents using the @racket[union-contents] procedure. Applications may use @racket[union?] and  
@racket[union-contents] directly to @tech[#:key "lifted constructs"]{lift} Racket code to work on
symbolic unions, but Rosette also provides dedicated lifting constructs, described in
the @seclink["sec:lifting-constructs"]{next section},
to make this process easier and the resulting lifted code more efficient.

@defproc[(union? [v any/c]) boolean?]{
Returns true if the given value is a symbolic union.  Otherwise returns false.  
@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define u (if b '(1 2) 3))
(union? u)
(union? b)]
}

@defproc[(union-contents [u union?]) (listof (cons/c (and/c boolean? term?) (not/c union?)))]{
Returns a list of guard-value pairs contained in the given union.  
@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define v (if b '(1 2) 3))
(union-contents v)]
}

@section[#:tag "sec:lifting-constructs"]{Symbolic Lifting}

Rosette provides two main constructs for @tech[#:key "lifted constructs"]{lifting}
Racket code to work on symbolic unions: @racket[for/all] and @racket[define-lift].
The @racket[for/all] construct is built into the language. It is used internally by Rosette 
to lift operations on @tech[#:key "unsolvable type"]{unsolvable types}.  The
@racket[define-lift] construct is syntactic sugar implemented on top of
@racket[for/all]; it is exported by the @racket[rosette/lib/lift] library.

@declare-exporting[rosette/base/core/forall rosette/lib/lift
                   #:use-sources (rosette/base/core/forall rosette/lib/lift)]

@defform[(for/all ([id val-expr]) body)]{
If @racket[val-expr] evaluates to a value that is not a @racket[union?], 
@racket[for/all] behaves like a @racket[let] expression.  It binds 
@racket[id] to the value and evaluates the @racket[body] with that binding.  

If @racket[val-expr] evaluates to a symbolic union, then for each 
guard-value pair @racket['(#, @var[g] . #, @var[v])] in that union, @racket[for/all]  
binds @racket[id] to @var[v] and evaluates the @racket[body] 
under the guard @var[g].  The results of the individual evaluations of 
the @racket[body] are re-assembled into a single (concrete or symbolic) 
output value, which is the result of the @racket[for/all] expression. 
If the evaluation of @racket[body] executes any procedure @var[p] that is neither  
implemented in nor provided by the @racket[rosette/safe] language, then @var[p] 
@bold{must be pure}---it may not perform any observable side-effects, 
such as writes to memory or disk.  There is no purity requirement for using procedures
that are implemented in or exported by @racket[rosette/safe] (e.g., @racket[vector-set!]).


The @racket[for/all] construct is useful both for lifting pure Racket procedures to work 
on symbolic unions and for controling the performance of Rosette's symbolic evaluation. 
The following examples show both use cases:

@itemlist[
@item{@emph{Lifting a pure Racket procedure 
to work on symbolic unions.}  

@defs+int[#:eval rosette-eval
[(require (only-in racket [string-length racket/string-length]))
 
(define (string-length value)
  (for/all ([str value]) 
    (racket/string-length str)))]

(string-length "abababa")
(string-length 3)
(define-symbolic b boolean?)
(string-length (if b "a" "abababa"))
(string-length (if b "a" 3))
(asserts)
(string-length (if b 3 #f))]}
           
@item{@emph{Making symbolic evaluation more efficient.}  @(rosette-eval '(clear-asserts!))
@defs+int[#:eval rosette-eval
[(require (only-in racket build-list))
 
(define limit 1000) 
 
(define (slow xs)
  (and (= (length xs) limit) (car (map add1 xs))))

(define (fast xs)
  (for/all ([xs xs]) (slow xs)))

(define ys (build-list limit identity))

(define-symbolic a boolean?)

(define xs (if a ys (cdr ys)))]

(time (slow xs))
(time (fast xs))]

Note that the above transformation will not always lead to better performance.  
Experimenting is the best way to determine whether and where to insert  
performance-guiding @racket[for/all]s.
}]}

@defform[(for*/all ([id val-expr] ...+) body)]{
Expands to a nested use of @racket[for/all], 
just like @racket[let*] expands to a nested use of @racket[let].}                                             
                                    
@defmodule[rosette/lib/lift #:no-declare]

@defform*[((define-lift id [(arg-type ...) racket-procedure-id])
           (define-lift id [arg-type racket-procedure-id]))]{
Binds @racket[id] to a procedure that lifts @racket[racket-procedure-id]  to 
work on symbolic unions.  In particular, the lifted procedure will work when given 
either a concrete Racket value or a symbolic union contains a guarded value of 
a suitable type, as given by @racket[arg-type].  Note that the lifted procedure 
will not work on symbolic terms, only on symbolic unions or concrete values.  The 
Racket procedure bound to @racket[racket-procedure-id] must be pure (see @racket[for/all]).

When @racket[racket-procedure-id] takes a specific number of arguments, 
the first form should be used, and the type of each argument should be given.
When @racket[racket-procedure-id] takes a variable number of arguments, 
the type of all arguments should be given.  Note that the second form omits 
the parentheses around the argument type to indicate a variable number of 
arguments, just like Racket's @racket[case-lambda] form.

The following example shows how to lift Racket's @racket[string-length] procedure 
to work on symbolic unions that contain strings.   

@defs+int[#:eval rosette-eval
[(require rosette/lib/lift)
(require (only-in racket [string-length racket/string-length] string?))

(define-lift string-length [(string?) racket/string-length])]

(string-length "abababa")
(define-symbolic b boolean?)
(string-length (if b "a" "abababa"))
(string-length (if b "a" 3))
(asserts)]
}

@(kill-evaluator rosette-eval)