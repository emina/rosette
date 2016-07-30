#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/solver/solution rosette/query/query rosette/query/eval 
           rosette/base/core/term rosette/lib/angelic
           (except-in rosette/query/debug assert )
           (only-in rosette/lib/synthax ?? choose define-synthax generate-forms print-forms)
           (only-in rosette/base/core/safe assert)
           (only-in rosette/base/base function? bitvector bvshl bvashr bvlshr bvadd bvsub bvmul)
           rosette/lib/render
           racket (only-in pict pict?))
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:rosette-libs"]{Solver-Aided Libraries}

In principle, solver-aided programming requires only symbolic values and the basic constructs described in Chapter @seclink["ch:syntactic-forms:rosette"]{3}.  In practice, however, it is often convenient to work with richer constructs, which are built on top of these primitives.  Rosette ships with three libraries that provide such constructs, as well as utility procedures for turning the results of synthesis and debugging queries into code.


@section{Synthesis Library}

@defmodule[rosette/lib/synthax #:use-sources (rosette/lib/synthax/core rosette/lib/synthax/form)]

@(rosette-eval '(require rosette/lib/synthax))
@defform[(?? maybe-type)
         #:grammar [(maybe-type (code:line)
                                type-expr)]
         #:contracts ([type-expr (and/c solvable? type? (not/c function?))])                                  
         ]{
Introduces a @tech{hole} into a program---a placeholder for a concrete constant of the given type.
The default type for holes, if one is not provided, is @racket[integer?]. 
Chapter @seclink["sec:synthesize"]{2.3.3} shows an example of using integer holes to @tech{sketch}
a factored polynomial function, which is then completed with the help of a @racket[synthesize]  query.  
The @racket[(??)] construct @seclink["sec:symbolic-constants"]{creates} 
and returns a fresh symbolic constant of type @racket[type-expr] (or @racket[integer?]).                
}

@defform[(choose expr ...+)]{
Introduces a choice @tech{hole} into a program---a placeholder to be filled with one of the given expressions. 
This construct defines @var[n]-1 fresh boolean constants and uses them to conditionally select one of the @var[n] 
provided expressions.    
@examples[#:eval rosette-eval
(define (div2 x)
  ([choose bvshl bvashr bvlshr bvadd bvsub bvmul] x (?? (bitvector 8))))
(define-symbolic i (bitvector 8))
(eval:alts
(print-forms 
 (synthesize #:forall (list i)
             #:guarantee (assert (equal? (div2 i) (bvudiv i (bv 2 8))))))
'(define (div2 x) (bvlshr x (bv 1 8))))] 
}

@defform*[((define-synthax id
             ([pattern expr] ...+))
           (define-synthax (id terminal ... k)
             #:base base-expr
             #:else else-expr))]{
Defines a grammar of expressions that can be used to 
fill holes of the form @racket[(id e ...)].  That is, writing 
@racket[(id e ...)] introduces a @tech{hole} that is to 
be filled with an expression from the @racket[id] grammar.

The first form, @racket[(define-synthax id ([pattern expr] ...+))], 
works like Racket's @racket[syntax-rules] macro:  it fills   
the hole @racket[(id e ...)] with the expression @racket[expr] that
corresponds to the first @racket[pattern] matching the hole.
The second form, @racket[(define-synthax (id terminal ... k) #:base base-expr #:else else-expr)],
defines a recursive grammar that is inlined a finite number of times
to fill a hole.  The @racket[base-expr] usually chooses among
the provided terminals,
while the @racket[else-expr] chooses among the terminals and a set of productions.
The hole @racket[(id e ... k)] must specify the inlining bound
@racket[k] as an integer literal.

@examples[#:eval rosette-eval
(eval:no-prompt
 (code:comment "Defines a grammar for boolean expressions")
 (code:comment "in negation normal form (NNF).")
 (define-synthax (nnf x y depth)
  #:base (choose x (! x) y (! y))
  #:else (choose
          x (! x) y (! y)
          ((choose && ||) (nnf x y (- depth 1))
                          (nnf x y (- depth 1))))))
(eval:no-prompt
 (code:comment "The body of nnf=> is a hole to be filled with an")
 (code:comment "expression of depth (up to) 1 from the NNF grammar.")
 (define (nnf=> x y)
   (nnf x y 1)))
(define-symbolic a b boolean?)
(eval:alts
 (print-forms
  (synthesize
   #:forall (list a b)
   #:guarantee (assert (equal? (=> a b) (nnf=> a b)))))
 `(define (nnf=> x y) (,|| (! x) y)))
]

Since @racket[define-synthax] uses macros to implement recursive grammars, 
instantiating a recursive grammar with a large limit (e.g., k > 3) can cause
long compilation times, especially if @racket[else-expr] contains many
recursive instantiations of the grammar.
}

@defproc[(generate-forms [solution solution?]) (listof syntax?)]{
Given a satisfiable @racket[solution?] to a @racket[synthesize]  query,
returns a list of @tech{sketch} completions for that query.  
Sketch completions can only be generated for programs that have been saved to disk.  
}

@defproc[(print-forms [solution solution?]) void?]{
  Pretty-prints the result of applying @racket[generate-forms] to the given  
  @racket[solution]. Sketch completions can only be generated and printed
 for programs that have been saved to disk. 
}

@section{Angelic Execution Library}
@defmodule[rosette/lib/angelic #:use-sources (rosette/lib/angelic)]

@(rosette-eval '(require rosette/lib/angelic))
@defproc[(choose* [v any/c] ...+) any/c]{
Non-determinstically chooses between the provided values. The difference
between @racket[choose*] and @racket[choose] is analogous to the difference
between @racket[define-symbolic*] and @racket[define-symbolic]:  the former
is dynamic and the latter is static.

@examples[#:eval rosette-eval
(define (static)  (choose 1 2 3))
(define (dynamic) (choose* 1 2 3))
(static)
(static)
(dynamic)
(dynamic)
(equal? (static) (static))
(equal? (dynamic) (dynamic))]
}

@section{Debugging Library}
@defmodule[rosette/lib/render #:use-sources (rosette/lib/render)]

@defproc[(render [solution solution?] [font-size natural/c 16]) pict?]{
Given an unsatisfiable @racket[solution?] to a @racket[debug]  query, returns a 
@racket[pict?] visualization of that solution.  The visualization displays the 
debugged code, highlighting the faulty expressions (i.e., those in the @racket[solution]'s core) in red. 
The optional @racket[font-size] parameter controls the size of the font used to typeset the code. 
Visualizations can only be constructed for programs that have been saved to disk.  
See Chapter @seclink["sec:debug"]{2.3.2} for an example of using @racket[render].
}



@(kill-evaluator rosette-eval)