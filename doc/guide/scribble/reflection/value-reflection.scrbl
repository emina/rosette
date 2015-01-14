#lang scribble/manual

@(require (for-label 
           rosette/solver/solver rosette/solver/solution rosette/query/state
           rosette/solver/kodkod/kodkod 
           rosette/base/define rosette/query/tools rosette/query/eval rosette/solver/solution
           rosette/base/term rosette/base/type rosette/base/primitive rosette/base/enum rosette/base/union
           rosette/base/forall rosette/lib/reflect/lift (only-in rosette/base/assert asserts)
           (only-in rosette/base/safe assert) 
           racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:value-reflection"]{Reflecting on Symbolic Values}

There are two kinds of symbolic values in Rosette:  @emph{symbolic terms} and 
@emph{symbolic unions}.  A Rosette program can inspect the representation of 
both kinds of values.  This is useful for @tech[#:key "lifted constructs"]{lifting} additional 
(unlifted) Racket procedures to work on symbolic values, and for 
controlling the performance of Rosette's symbolic evaluator.

@section[#:tag "sec:symbolic-terms"]{Symbolic Terms}

A symbolic term is either a symbolic constant, created via @racket[define-symbolic], 
or a symbolic expressions, produced by applying a lifted operator to one or more 
symbolic terms.  Terms are strongly typed. The only types that include symbolic terms 
as values are @tech[#:key "primitive datatype"]{primitive datatypes} and programmer-defined 
@seclink["sec:enum"]{enumerations}.  Symbolic values of all other types take the form of 
@seclink["sec:symbolic-unions"]{symbolic unions}.

@declare-exporting[rosette/base/term #:use-sources (rosette/base/type rosette/base/op rosette/base/term)]

@defproc[(type? [value any/c]) boolean?]{
Returns true when given a predicate that recognizes a @seclink["ch:built-in-datatypes"]{built-in type}, a programmer-defined @seclink["sec:enum"]{enumeration}, 
or a programmer-defined @seclink["sec:struct"]{structure} type.  Otherwise returns false.
@examples[#:eval rosette-eval
(type? number?)
(type? boolean?)
(type? list?)
(define-enum suit '(club diamond heart spade))
(type? suit?)
(type? 1)]
}

@defproc*[([(type-of [value any/c]) type?])]{
Returns the most specific @racket[type?] predicate that accepts the given @racket[value].
@examples[#:eval rosette-eval
(define-symbolic x number?)
(type-of x) 
(type-of (+ x 1))
(type-of #t)]
}

@deftogether[(@defproc[(term? [value any/c]) boolean?]
              @defproc[(expression? [value any/c]) boolean?]
              @defproc[(constant? [value any/c]) boolean?])]{
Predicates for recognizing symbolic terms, expressions, and constants, respectively.
@examples[#:eval rosette-eval
(code:line (define-symbolic x number?) (code:comment "constant"))
(code:line (define e (+ x 1)) (code:comment "expression"))
(list (term? x) (term? e))
(list (constant? x) (constant? e))
(list (expression? x) (expression? e))
(term? 1)]
}

@defproc*[([(term-name [value constant?]) (or/c syntax? (cons/c syntax? any/c))]
           [(term-name [value any/c]) #f])]{
Given a @racket[constant?] term, returns the unique identifier for that term.  
This identifier may be a syntax object or a pair consisting of a 
syntax object and another value (e.g., a natural number).   
@examples[#:eval rosette-eval
(define-symbolic x number?)
(define-symbolic* b boolean?)
(term-name x)
(term-name b)
(term-name (+ x 1))
(term-name 1)]
}


@defproc*[([(term-op [value expression?]) any/c]
           [(term-op [value any/c]) #f])]{
Given an @racket[expression?] term, returns a value that represents 
its operator.  The operator value is @racket[equal?] to the lifted 
procedure used to construct the value, but they are not the same object, 
and the output of @racket[term-op] should not be used as a procedure by Rosette programs.
@examples[#:eval rosette-eval
(define-symbolic x number?)
(term-op x)
(term-op (+ x 1))
(term-op 1)]
}

@defproc*[([(term-child [value expression?]) (listof any/c)]
           [(term-child [value any/c]) #f])]{
Given an @racket[expression?] term, returns the list of its children.  
At least one child in this list is itself a @racket[term?], and all children 
in the list have a @tech[#:key "primitive datatype"]{primitive} or 
@seclink["sec:enum"]{enumeration} type.  The number of children and 
their types are determined by the expression's operator.
@examples[#:eval rosette-eval
(define-symbolic x number?)
(term-op x)
(term-child (+ x 1)) 
(term-child 1)]
}

@defproc*[([(term-property [t term?] [prop any/c] [value any/c]) term?]
           [(term-property [t term?] [prop any/c]) any/c])]{
Each term can be annotated with any number of property-value pairs.  
The three-argument version of @racket[term-property]
returns a fresh copy of the term @racket[t], annotated with the given property-value pair. 
The two-argument version returns the value that the term @racket[t] associates with the property @racket[prop], 
or @racket[#f] if @racket[t] has no value for @racket[prop].
}

@defproc*[([(term-track-origin [t term?] [origin any/c]) term?]
           [(term-origin [t term?]) any/c])]{
Functionally sets and retrieves the distinguished @racket['origin] 
property of a term.  See @racket[term-property].
}

@defproc*[([(term->datum [t term?]) any/c])]{
Returns a plain Racket datum that corresponds to the given term.
Expressions are converted into lists, and constants are converted 
into symbols.  The output of @racket[term->datum] is suitable for pretty-printing. 
@examples[#:eval rosette-eval
(define-symbolic x number?)
(define-symbolic* b boolean?)
(term->datum x)
(term->datum b)
(term->datum (+ x 1))]
}


@section[#:tag "sec:symbolic-unions"]{Symbolic Unions}

@declare-exporting[rosette/base/union #:use-sources (rosette/base/union)]

Rosette represents a symbolic value of a @tech[#:key "composite datatype"]{composite datatype} (such as a list or a programmer-defined structure) as a union of @deftech{guarded values} of that type. A guarded value is a pair that combines a guard, which is a symbolic boolean term, and another (non-union) value.  The guards in a symbolic union are, by construction, disjoint:  only one of them can ever be true.   For example, the symbolic vector @racket[s] defined below is represented as a symbolic union of two guarded vectors:
@interaction[#:eval rosette-eval
(define-symbolic b boolean?)
(define v (vector 1))
(define w (vector 2 3))
(define s (if b v w))
s
(type-of s)
(eq? s v)
(eq? s w)]

The values that appear in a union are themselves never unions.  They may, however, contain unions.  They may also belong to several different types.  In that case, the type of the union is the most specific @racket[type?] predicate that accepts all members of the union.  This will always be a composite type---possibly, the most general composite type @racket[any/c].
@interaction[#:eval rosette-eval
(define-symbolic b boolean?)
(define-symbolic c boolean?)
(define v (if c "c" 0))
(define u (if b (vector v) 4))
u
(type-of u)]

Symbolic unions are recognized with the @racket[union?] predicate, and Rosette programs can inspect their contents using the @racket[union-contents] procedure. These two procedures may be used directly to @tech[#:key "lifted constructs"]{lift} Racket code to work on symbolic unions, but Rosette also provides dedicated lifting constructs, described in the @seclink["sec:lifting-constructs"]{next section}, that make this process easier and the resulting lifted code more efficient.

@defproc[(union? [value any/c]) boolean?]{
Returns true if the given value is a symbolic union.  Otherwise returns false.  
@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define u (if b '(1 2) 3))
(union? u)
(union? b)]
}

@defproc[(union-contents [value union?]) (listof (cons/c (and/c boolean? term?) (not/c union?)))]{
Returns a list of guard-value pairs contained in the given union.  
@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define u (if b '(1 2) 3))
(union-contents u)]
}  

@section[#:tag "sec:lifting-constructs"]{Constructs for Symbolic Lifting}


Rosette provides two main constructs for @tech[#:key "lifted constructs"]{lifting} Racket code to work on symbolic unions: @racket[for/all] and @racket[define-lift]. The @racket[for/all] construct is built into the language. It is used in Rosette's internal code for lifting operations on @tech[#:key "composite datatype"]{composite datatypes}.  The @racket[define-lift] construct is syntactic sugar implemented on top of @racket[for/all]; it is exported by the @racket[rosette/lib/reflect/lift] library.

@declare-exporting[rosette/base/forall rosette/lib/reflect/lift #:use-sources (rosette/base/forall rosette/lib/reflect/lift)]

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
           
@item{@emph{Making symbolic evaluation more efficient.}  @(rosette-eval '(clear-asserts))
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
}]

}


@defform[(for/all* ([id val-expr] ...+) body)]{
Expands to a nested use of @racket[for/all], 
just like @racket[let*] expands to a nested use of @racket[let].
                                           
}                                             
                                    
@defmodule[rosette/lib/reflect/lift #:no-declare]

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
arguments, just like Racket's case-lambda form.


The following example shows how to lift Racket's @racket[string-length] procedure 
to work on symbolic unions that contain strings.   

@defs+int[#:eval rosette-eval
[(require rosette/lib/reflect/lift)
(require (only-in racket [string-length racket/string-length] string?))

(define-lift string-length [(string?) racket/string-length])]

(string-length "abababa")
(define-symbolic b boolean?)
(string-length (if b "a" "abababa"))
(string-length (if b "a" 3))
(asserts)]
}
@(kill-evaluator rosette-eval)