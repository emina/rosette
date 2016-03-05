#lang scribble/manual

@(require (for-label 
           rosette/base/define rosette/solver/solution rosette/query/tools rosette/query/eval 
           rosette/base/term rosette/base/enum 
           (except-in rosette/query/debug false true)
           (only-in rosette/lib/meta/constructs ?? choose define-synthax)
           (only-in rosette/lib/meta/generate generate-expressions generate-forms)
           (only-in rosette/lib/meta/display print-expressions print-forms)
           (only-in rosette/base/base << >> >>>)
           (only-in rosette/base/safe assert) 
           rosette/lib/tools/render
           racket (only-in pict pict?))
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))



@title[#:tag "sec:rosette-libs"]{Solver-Aided Libraries}

In principle, solver-aided programming requires only symbolic values and the basic constructs described in Chapter @seclink["ch:syntactic-forms:rosette"]{3}.  In practice, however, it is often convenient to work with richer constructs, which are built on top of these primitives.  Rosette ships with two libraries that provide such constructs, as well as utility procedures for turning the results of synthesis and debugging queries into code.

@section{Synthesis Library}

@defmodule[rosette/lib/meta/meta #:use-sources (rosette/lib/meta/constructs rosette/lib/meta/generate rosette/lib/meta/display)]

@defform[(??)]{
Introduces an integer @tech{hole} into a program---a placeholder for a concrete integer constant. 
Chapter @seclink["sec:synthesize"]{2.3.3} shows an example of using integer holes to @tech{sketch}
a factored polynomial function, which is then completed with the help of a @racket[synthesize]  query.  
The @racket[(??)] construct @seclink["sec:symbolic-constants-and-assertions"]{creates} 
and returns a fresh symbolic constant of type @racket[number?].                
}

@(rosette-eval '(require rosette/lib/meta/meta))
@defform[(choose expr ...+)]{
Introduces a choice @tech{hole} into a program---a placeholder to be filled with one of the given expressions. 
This construct defines @var[n]-1 fresh boolean constants and uses them to conditionally select one of the @var[n] 
provided expressions.    
@examples[#:eval rosette-eval
(define (div2 x) ([choose >> >>> << + - *] x (??)))
(define-symbolic i number?)
(eval:alts
(print-forms 
 (synthesize #:forall (list i)
             #:assume (assert (>= i 0))
             #:guarantee (assert (= (div2 i) (quotient i 2)))))
'(define (div2 x) (>> x 1)))
] 
}

@defform[(define-synthax (id arg ...) maybe-guard body)
         #:grammar 
         ([maybe-guard (code:line) (code:line #:assert guard)])]{
Defines a grammar of expressions that can be used to 
fill holes of the form @racket[(id expr ...)].  That is, writing 
@racket[(id expr ...)] introduces a @tech{hole} that is to 
be filled with an expression from the @racket[id] grammar.

@examples[#:eval rosette-eval

(code:comment "Defines the following grammar:")
(code:comment "  shift := terminal ... | const | (op shift shift)")
(code:comment "     op := >> | << | >>>")
(code:comment "  const := (??)")
(define-synthax (shift terminal ... k)
  #:assert (>= k 0)
  [choose
   terminal ... (??)
   ([choose >> << >>>] (shift terminal ... (- k 1)) 
                       (shift terminal ... (- k 1)))])

(code:comment "A sketch with a hole to be filled with a shift expression of depth <= 2.")
(define (div2mul4 x) (shift x 2))

(define-symbolic i number?)
(eval:alts
 (print-forms
  (synthesize #:forall (list i)
              #:assume (assert (>= i 0))
              #:guarantee (assert (= (div2mul4 i) (* 4 (quotient i 2))))))
'(define (div2mul4 x) (<< (>>> x 1) 2)))
] 

Recursive grammars, such as @racket[shift], must be equipped with 
a @racket[guard] that limits the size of a hole expression drawn 
from the grammar.  Since @racket[define-synthax] uses macros to implement recursive grammars, 
instantiating a recursive grammar with a large limit (e.g., k > 3) can cause long compilation times.   
The @racket[define-synthax] construct may be changed in the future to a more efficient 
procedure-based implementation.
}

@(rosette-eval '(require (only-in racket datum->syntax)))

@defproc[(generate-expressions [solution solution?]) (listof (cons/c syntax? syntax?))]{
Given a satisfiable @racket[solution?] to a @racket[synthesize]  query, returns a list that 
associates each hole involved in the query with a synthesized expression. Hole completions 
can only be generated for programs that have been saved to disk.  In the 
following example, @racket[generate-expressions] returns a list that associates the 
@racket[choose] hole (line 1, column 19) with the expression @racket[>>], and the 
@racket[??] hole (line 1, column 46) with the expression @racket[1].
@examples[#:eval rosette-eval
(define (div2 x) ([choose >> >>> << + - *] x (??))) 
(define-symbolic i number?)
(eval:alts
(generate-expressions
 (synthesize #:forall (list i)
             #:assume (assert (>= i 0))
             #:guarantee (assert (= (div2 i) (quotient i 2)))))
(list (cons (datum->syntax #f 'choose (list #f 1 19 #f #f)) (datum->syntax #f '>>))
      (cons (datum->syntax #f '?? (list #f 1 46 #f #f)) (datum->syntax #f '1))))
] 
}

@defproc[(generate-forms [solution solution?]) (listof (cons/c syntax? syntax?))]{
Given a satisfiable @racket[solution?] to a @racket[synthesize]  query, returns a list that 
associates each top-level @tech{sketch} involved in the query with a completion of that sketch.  
Sketch completions can only be generated for programs that have been saved to disk.  
In the following example, @racket[generate-forms] returns a list that associates the 
@racket[div2] sketch (line 2, column 1) with its synthesized completion.
@examples[#:eval rosette-eval
(define (div2 x) ([choose >> >>> << + - *] x (??))) 
(define-symbolic i number?)
(eval:alts
(generate-forms
 (synthesize #:forall (list i)
             #:assume (assert (>= i 0))
             #:guarantee (assert (= (div2 i) (quotient i 2)))))
(list (cons (datum->syntax #f 'define (list #f 2 1 #f #f)) (datum->syntax #f  '(define (div2 x) (>> x 1))))))
]
}

@deftogether[(@defproc[(print-expressions [solution solution?]) void?]
              @defproc[(print-forms [solution solution?]) void?])]{
  Pretty-prints the result of applying 
  @racket[generate-expressions] or @racket[generate-forms] to the given  
  @racket[solution].     
}

@section{Debugging Library}
@defmodule[rosette/lib/tools/render #:use-sources (rosette/lib/tools/render)]

@defproc[(render [solution solution?] [font-size natural/c 16]) pict?]{
Given an unsatisfiable @racket[solution?] to a @racket[debug]  query, returns a 
@racket[pict?] visualization of that solution.  The visualization displays the 
debugged code, highlighting the faulty expressions (i.e., those in the @racket[solution]'s minimal core) in red. 
The optional @racket[font-size] parameter controls the size of the font used to typeset the code. 
Visualizations can only be constructed for programs that have been saved to disk.  
See Chapter @seclink["sec:debug"]{2.3.2} for an example of using @racket[render].
}

@(kill-evaluator rosette-eval)