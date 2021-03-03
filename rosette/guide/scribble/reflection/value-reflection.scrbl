#lang scribble/manual

@(require (for-label 
           rosette/solver/solver rosette/solver/solution rosette/query/query
           rosette/base/form/define  
           rosette/base/core/term rosette/base/core/type rosette/base/core/union
           (only-in rosette/base/base vc clear-vc! bitvector ~>)
           (only-in rosette/base/core/reflect symbolics concrete? symbolic?)
           rosette/base/core/forall racket)
          scribble/core scribble/html-properties scribble/example racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:value-reflection"]{Reflecting on Symbolic Values}

There are two kinds of symbolic values in Rosette: symbolic
terms and symbolic unions. A Rosette program can inspect the
representation of both kinds of values. This is useful for
@tech[#:key "lifted constructs"]{lifting} additional
(unlifted) Racket procedures to work on symbolic values, and
for controlling the performance of Rosette's symbolic
evaluator.

@section[#:tag "sec:symbolic-terms"]{Symbolic Terms}

@declare-exporting[rosette/base/core/term rosette/base/core/reflect
                   #:use-sources (rosette/base/core/type rosette/base/core/term rosette/base/core/reflect)]

A @deftech{symbolic term} is either a symbolic constant,
created via @seclink["sec:symbolic-constants"]{@code{define-symbolic[*]}},
or a symbolic expression, produced by a lifted operator.
Terms are strongly typed, and always belong to a @tech{solvable type}.
Symbolic values of all other
(@tech[#:key "unsolvable type"]{unsolvable}) types take the
form of @seclink["sec:symbolic-unions"]{symbolic unions}.

@deftogether[(@defproc[(term? [v any/c]) boolean?]
              @defproc[(expression? [v any/c]) boolean?]
              @defproc[(constant? [v any/c]) boolean?])]{
                                                         
 Predicates for recognizing symbolic terms, expressions, and
 constants, respectively.

@examples[#:eval rosette-eval
(code:line (define-symbolic x integer?) (code:comment "Constant."))
(code:line (define e (+ x 1))           (code:comment "Expression."))
(list (term? x) (term? e))
(list (constant? x) (constant? e))
(list (expression? x) (expression? e))
(term? 1)]
}

@(rosette-eval '(require racket/match))
@deftogether[(@defform[(term content type)]
              @defform[(expression op child ...+)]
              @defform[(constant id type)])]{
                                             
 Pattern matching forms for symbolic terms, expressions, and
 constants, respectively.

@examples[#:eval rosette-eval
(code:line (define-symbolic x integer?) (code:comment "Constant."))
(code:line (define e (+ x 1))           (code:comment "Expression."))
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
(symbolics (list (vector (box 1) 3) (cell (cons 4 5))))
(symbolics (list 5 (cons (box y) z) (vector (cell x) z)))]
}

@defproc[(concrete? [v any/c]) boolean?]{
Equivalent to @racket[(null? (symbolics v))] but more efficient.
@examples[#:eval rosette-eval
(define-symbolic x y z integer?) 
(concrete? x)
(concrete? (if (= x y) 2 #f))
(concrete? (list y z y))
(struct cell (value) #:transparent)
(concrete? (list (vector (box 1) 3) (cell (cons 4 5))))
(concrete? (list 5 (cons (box y) z) (vector (cell x) z)))]              
}

@defproc[(symbolic? [v any/c]) boolean?]{
Equivalent to @racket[(not (concrete? v))].
}

@defproc*[([(type-of [v any/c] ...+) type?])]{
                                              
 Returns the most specific @racket[type?] predicate that
 accepts all of the given values.
                          
@examples[#:eval rosette-eval
(define-symbolic x integer?)
(type-of x) 
(type-of (+ x 1))
(type-of x 3.14)
(type-of #t)
(type-of #t 1)]
}

@defproc[(type? [v any/c]) boolean?]{
                                     
 Returns true when given a predicate that recognizes a
 @seclink["ch:built-in-datatypes"]{built-in} or a
 @seclink["ch:programmer-defined-datatypes"]{structure} type.
 Otherwise returns false.

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
(solvable? (bitvector 8))
(solvable? (~> (bitvector 32) (bitvector 64)))
(solvable? list?)
(struct circle (radius))
(solvable? circle?)
(solvable? any/c)
]}

@section[#:tag "sec:symbolic-unions"]{Symbolic Unions}

@declare-exporting[rosette/base/core/union #:use-sources (rosette/base/core/union)]

Rosette represents symbolic values of an
@tech[#:key "unsolvable type"]{unsolvable type} (e.g.,
@racket[list?]) as @deftech[#:key "symbolic union"]{symbolic unions}.
A symbolic union is a set of two or more
@deftech[#:key "guarded value"]{guarded values}. A guarded
value, in turn, combines a guard, which is a symbolic
@racket[boolean?] term, and a (non-union) value. Rosette's
symbolic evaluator guarantees that the guards in a symbolic
union are disjoint: only one of them can ever be true. For
example, the symbolic vector @racket[s] defined below is
represented as a symbolic union of two guarded vectors:


@examples[#:eval rosette-eval #:label #f
(define-symbolic b boolean?)
(define v (vector 1))
(define w (vector 2 3))
(define s (if b v w))
s
(type-of s)
(eq? s v)
(eq? s w)]

The values that appear in a union are themselves never
unions. They may, however, contain unions. They may also
belong to several different types. In that case, the type of
the union is the most specific @racket[type?] predicate that
accepts all members of the union. This will always be an
unsolvable type---possibly @racket[any/c], the most general
unsolvable type.

@examples[#:eval rosette-eval #:label #f
(define-symbolic b boolean?)
(define-symbolic c boolean?)
(define v (if c "c" 0))
(define u (if b (vector v) 4))
u
(type-of u)
(union-contents u)]

Symbolic unions are recognized by the @racket[union?]
predicate, and Rosette programs can inspect union contents
using the @racket[union-contents] procedure. Applications
may use @racket[union?] and @racket[union-contents] directly
to @tech[#:key "lifted constructs"]{lift} Racket code to
work on symbolic unions, but Rosette also provides dedicated
lifting constructs, described in the
@seclink["sec:lifting-constructs"]{next section}, to make
this process easier and the resulting lifted code more
efficient.

@defproc[(union? [v any/c]) boolean?]{
                                      
 Returns true if the given value is a symbolic union.
 Otherwise returns false.

@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define u (if b '(1 2) 3))
(union? u)
(union? b)]
}

@defproc[(union-contents [u union?]) (listof (cons/c (and/c boolean? term?) (not/c union?)))]{
                                                                                              
 Returns a list of guard-value pairs contained in the given
 union.

@examples[#:eval rosette-eval
(define-symbolic b boolean?)
(define v (if b '(1 2) 3))
(union-contents v)]
}


@section[#:tag "sec:lifting-constructs"]{Symbolic Lifting}

Rosette provides a construct, @racket[for/all], for
controlling how symbolic evaluation is performed. This
construct is built into the language. It is used internally
by Rosette to @tech[#:key "lifted constructs"]{lift}
operations on @tech[#:key "unsolvable type"]{unsolvable
 types}. Rosette programs can also use it for this purpose,
as well as for tuning the performance of symbolic
evaluation.


@declare-exporting[rosette/base/core/forall rosette/lib/lift
                   #:use-sources (rosette/base/core/forall rosette/lib/lift)]

@defform[(for/all ([id val-expr maybe-exhaustive]) body ...+)
         #:grammar [(maybe-exhaustive (code:line) #:exhaustive)]]{

 Splits the evaluation of @racket[body ...+] into multiple
 paths, based on the value of @racket[val-expr]. In
 particular, @racket[for/all] evaluates @racket[val-expr],
 decomposes the resulting value @var{V} into a list of
 guard-value pairs @racket[(g v) ...+], and evaluates the equivalent of
 the following expression:
 @(racketblock
   (cond [g (let ([id v]) body ...)] ...))

 The keyword @racket[#:exhaustive] controls the
 decomposition of @var{V}.
 
 When @racket[#:exhaustive] is omitted, @var{V} is decomposed into
 @racket[(union-contents #, @var{V})] if it is a symbolic
 union. Otherwise, it is decomposed trivially into
 @racket[(list (cons #t #, @var{V}))].
 
 @examples[#:eval rosette-eval
 (define-symbolic b boolean?)

 (define u (if b (list 1 2) (list 3)))
 (code:line
 u                      (code:comment "u is a symbolic union,"))
 (for/all ([v u])       (code:comment "so the body of for/all is evaluated")
   (printf "~a, " (vc)) (code:comment "under each of its guards,")
   (printf "~a\n" v)    (code:comment "with the corresponding value bound to v.")
   (map add1 v))

 (vc)
 
 (define i (if b 5 10))
 (code:line
 i                      (code:comment "i is not a union,"))
 (for/all ([v i])       (code:comment "so the body of for/all is evaluated")
   (printf "~a, " (vc)) (code:comment "once under the trival #t guard,")
   (printf "~a\n" v)    (code:comment "with i itself bound to v.")
   (add1 v))

 (vc)]

 When @racket[#:exhaustive] is included, @var{V} is
 decomposed into its constituent guard-value pairs
 recursively if it is either a union or a conditional
 symbolic term. Otherwise, it is decomposed trivally.
          
 @examples[#:eval rosette-eval
 (code:comment "i is a conditional term, so with #:exhaustive, it is")
 (code:comment "split into its constituent guard-value pairs.")
 (for/all ([v i #:exhaustive]) 
   (printf "~a, " (vc))     
   (printf "~a\n" v)     
   (add1 v))
 (code:comment "The #:exhaustive decomposition is done recursively,")
 (code:comment "until no more decomposable terms are left.")
 (define-symbolic a c boolean?)
 (define j (if c i 0))
 j
 (define u (if a (list 1 2) j))
 u
 (for/all ([v u #:exhaustive]) 
   (printf "~a, " (vc))     
   (printf "~a\n" v)     
   (add1 v))
 (code:comment "The path guarded by a leads to failure,")
 (code:comment "as reflected in the resulting vc.")
 (vc)
 (clear-vc!)
 (code:comment "The above for/all expression outputs a term")
 (code:comment "that is syntactically different from but")
 (code:comment "semantically equivalent to the output of (add1 u),")
 (code:comment "with the same effect on the vc.")
 (add1 u)
 (vc)
 (clear-vc!)]

 Programs can use @racket[for/all] to lift pure Racket
 procedures to work on symbolic values and to control the performance
 of Rosette's symbolic evaluation.

 For example, the following snippet shows how to use
 @racket[for/all] to lift @racket[string-length]
 to work on symbolic strings. This approach
 generalizes to all unlifted datatypes, since their
 symbolic representation is always a union of guarded
 concrete values.

 @examples[#:eval rosette-eval #:label #f

 (eval:no-prompt
  (require (only-in racket [string-length racket/string-length])))

 (eval:no-prompt
  (define (string-length value)
    (for/all ([str value]) 
      (racket/string-length str))))

  (string-length "abababa")
  (eval:error (string-length 3))
  (define-symbolic b boolean?)
  (string-length (if b "a" "abababa"))
  (vc)
  (string-length (if b "a" 3))
  (vc)
  (clear-vc!)]

 Here is also an example of using @racket[for/all] for
 performance tuning. Note that adding @racket[for/all]s in
 too many places, or in the wrong places, will lead to worse
 performance. @seclink["ch:performance"]{Profiling} is the
 best way to determine whether and where to insert them.

 @examples[#:eval rosette-eval #:label #f
 (eval:no-prompt
  (require (only-in racket build-list)))
 
 (eval:no-prompt
  (define (vector-update! v idx proc)  (code:comment "v[idx] := proc(v[idx])")
    (vector-set! v idx (proc (vector-ref v idx)))))
 
 (eval:no-prompt
  (define (vector-update!! v idx proc) 
    (for/all ([idx idx #:exhaustive])  (code:comment "Split paths based on idx.")
      (vector-update! v idx proc))))

 (define-symbolic a b boolean?)
 (define idx (if a 0 (if b 5 10)))
 (define limit 10000)
 (code:line (define vals (build-list limit identity)) (code:comment "'(0 1 ... 9999)"))

 (code:comment "vector-update! is 2 orders of magnitude slower than")
 (code:comment "vector-update!! on the following tests.")
 (code:comment "By default, Rosette treats idx as an opaque symbolic")
 (code:comment "integer when evaluating vector-update!, so it must")
 (code:comment "account for the possibility of idx taking on any value")
 (code:comment "between 0 and limit. The for/all in vector-update!!")
 (code:comment "instructs Rosette to split the index and consider only")
 (code:comment "the values that idx can in fact take on: 0, 5, 10.")
 (define v0 (list->vector vals))
 (time (vector-update! v0 idx add1))
 (define v1 (list->vector vals))
 (time (vector-update!! v1 idx add1))
 (code:comment "But the default evaluation strategry is better for indices")
 (code:comment "that are fully symbolic conditional terms.")
 (define-symbolic i0 i1 i2 i3 integer?)
 (define idx (if a (if b i0 i1) (if b i2 i3)))
 (define v2 (list->vector vals))
 (time (vector-update! v2 idx add1))
 (define v3 (list->vector vals))
 (time (vector-update!! v3 idx add1))]}

@defform[(for*/all ([id val-expr maybe-exhaustive]) body ...+)
         #:grammar [(maybe-exhaustive (code:line) #:exhaustive)]]{
                                                    
 Expands to a nested use of @racket[for/all], just like
 @racket[let*] expands to a nested use of @racket[let].

}                                             
                                                         

@(kill-evaluator rosette-eval)