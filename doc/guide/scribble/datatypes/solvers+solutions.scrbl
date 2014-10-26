#lang scribble/manual

@(require (for-label 
           rosette/solver/solver rosette/solver/solution rosette/query/state
           rosette/solver/kodkod/kodkod (only-in rosette/query/debug debug)
           rosette/solver/smt/z3 rosette/solver/smt/cvc4
           rosette/base/define rosette/query/tools rosette/query/eval rosette/solver/solution
           rosette/base/term (only-in rosette/base/num current-bitwidth) rosette/base/primitive
           (only-in rosette/base/safe assert) 
           racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:solvers-and-solutions"]{Solvers and Solutions}

@declare-exporting[rosette/query/eval
                   rosette/solver/solver
                   rosette/solver/solution 
                   rosette/query/state    
                   rosette/solver/kodkod/kodkod 
                   rosette/solver/smt/z3    
                   rosette/solver/smt/cvc4
                   #:use-sources 
                   (rosette/query/eval rosette/solver/solver rosette/solver/solution rosette/query/state rosette/solver/kodkod/kodkod rosette/solver/smt/z3 rosette/solver/smt/cvc4)]

A @deftech{solver} is an automatic reasoning engine, used to answer 
@seclink["sec:queries"]{queries} about Rosette programs.  The result of
a solver invocation is a @deftech{solution}, containing either 
a @tech{binding} of symbolic constants to concrete values, or 
an @tech[#:key "MUC"]{unsatisfiable core}. 
Solvers and solutions may not be symbolic.  Two solvers (resp. solutions) are @racket[eq?]/@racket[equal?] 
if they refer to the same object.

@section{The Solver Interface and Classes}


@defparam[current-solver solver (is-a?/c solver<%>)]{
  The @racket[current-solver] parameter holds the solver object used for 
  answering solver-aided queries.  If a query requires creation of additional 
  temporary solvers, they all have the same @racket[class?] as the @racket[current-solver].
  Supported solvers include @racket[kodkod%] and, if  
  @seclink["sec:get"]{installed}, @racket[z3%] and @racket[cvc4%].
  @examples[#:eval rosette-eval
   (eval:alts (current-solver) (display (current-solver)))
   (require rosette/solver/smt/z3 rosette/solver/smt/cvc4 (only-in racket new))
   (code:line (current-solver (new z3%)) (code:comment "change the current solver"))
   (eval:alts (current-solver) (display (current-solver)))
   (code:line (current-solver (new cvc4%)) (code:comment "change it again"))
   (eval:alts (current-solver) (display (current-solver)))]
}

@(rosette-eval '(require rosette/solver/kodkod/kodkod))
@(rosette-eval '(current-solver (new kodkod%)))

@definterface[solver<%> ()  
@elem{The solver interface specifies basic operations for 
      posing and answering questions about the satisfiability of a set of 
      formulas, expressed as (symbolic) boolean values.  As a general rule,
      Rosette programs should not invoke these operations directly.  The recommended
      way to access the solver is by posing @seclink["sec:queries"]{solver-aided queries}.}
@defmethod[(assert [formula boolean?]...) void?]{
Adds the given formulas to the solver's worklist.}
@defmethod[(clear) void?]{
Clears the solver's worklist.}
@defmethod[(solve) solution?]{
Searches for a binding from symbolic constants to concrete values that 
satisfies all assertions in the solver's worklist.  If such a binding---or, a @racket[model]---exists, 
it is returned in the form of a satisfiable (@racket[sat?]) solution.  Otherwise, 
an unsatisfiable (@racket[unsat?]) solution is returned, but without 
computing an unsatisfiable core.  A solution with a core can be obtained by calling 
@racket[debug] on @(this-obj). }
@defmethod[(debug) solution?]{
Searches for a minimal unsatisfiable core of the assertions in the solver's worklist.  
If the worklist assertions are satisfiable, or @(this-obj) does 
not support core extraction, an error is thrown.  Otherwise, the result is an 
@racket[unsat?] solution with a minimal @racket[core].}                              
]

@defmodule[#:multi (rosette/solver/kodkod/kodkod) #:no-declare #:use-sources (rosette/solver/kodkod/kodkod)] 
@defclass[kodkod%  object% (solver<%>)  
@elem{A Rosette front-end to the @hyperlink["http://alloy.mit.edu/kodkod/"]{Kodkod} solver.  This solver supports
minimal core extraction.}]

@defmodule[#:multi (rosette/solver/smt/z3) #:no-declare #:use-sources (rosette/solver/smt/z3)] 
@defclass[z3%  object% (solver<%>)  
@elem{A Rosette front-end to the @hyperlink["http://z3.codeplex.com"]{Z3} solver from Microsoft.
This solver does not support minimal core extraction.}]

@defmodule[#:multi (rosette/solver/smt/cvc4) #:no-declare #:use-sources (rosette/solver/smt/cvc4)] 
@defclass[cvc4%  object% (solver<%>)  
@elem{A Rosette front-end to the @hyperlink["http://cvc4.cs.nyu.edu/web/"]{CVC4} solver from NYU.
This solver does not support minimal core extraction.}]


@section{Satisfiable and Unsatisfiable Solutions}

A solution to a set of formulas consists of either a @racket[model], 
if the formulas are satisfiable, or a @racket[core], if they are not. 
The @racket[sat?] and @racket[unsat?] predicates recognize 
satisfiable and unsatisfiable solutions, respectively.  A satisfiable solution 
can be used as a procedure:  when applied to a bound symbolic constant, it returns 
a concrete value for that constant; when applied to any other value, it returns 
the value itself.

A solution supports the following operations:

@defproc[(solution? [value any/c]) boolean?]{
Returns true iff the given @racket[value] is a solution.}

@defproc[(sat? [solution solution?]) boolean?]{
Returns true iff the given @racket[solution] is satisfiable.}

@defproc[(unsat? [solution solution?]) boolean?]{
Returns true iff the given @racket[solution] is unsatisfiable.}

@defproc[(sat [binding (hash/c constant? any/c #:immutable #t)]) solution?]{
Returns a satisfiable solution that holds the given binding from symbolic 
constants to values.  The provided hashmap must bind every symbolic constant 
in its keyset to a concrete value of the same type.
}

@defproc*[([(unsat) solution?]
           [(unsat [a-core (listof boolean?)]) solution?])]{
Returns an unsatisfiable solution.  If @racket[a-core] is provided, 
it must be a list of boolean values that are collectively unsatisfiable.  
Otherwise, the @racket[core] of the produced solution is 
set to #f, to indicate that there is no satisfying solution but
core extraction was not performed.  (Core extraction is an expensive 
operation that is not supported by all solvers; those that do support it 
usually don't compute a core unless explicitly asked for one.)}

@defproc[(empty-solution) solution?]{
Returns a satisfiable solution with an empty binding as a @racket[model].}

@defproc[(model [solution solution?]) (or/c (hash/c constant? any/c #:immutable #t) #f)]{
Returns the binding stored in the given solution.  If the solution is 
@racket[sat?], the binding is an immutable hashmap from symbolic constants 
to values.  Otherwise, the binding is @racket[#f].
}

@defproc[(core [solution solution?]) (or/c (listof (and/c constant? boolean?)) #f)]{
Returns unsatisfiable core stored in the given solution.  If the solution is 
@racket[unsat?] and a core was computed, the result is a list of boolean values that 
are collectively unsatisfiable.  Otherwise, the result is @racket[#f].
}

@defproc[(evaluate [value any/c] [solution (and/c solution? sat?)]) any/c]{
Given a Rosette value and a satisfiable solution, @racket[evaluate] produces a 
new value obtained by replacing every symbolic constant @var[c] in @racket[value] 
with @racket[(solution #, @var[c])] and simplifying the result.
@examples[#:eval rosette-eval                
(define-symbolic a b boolean?)
(define-symbolic x y number?)
(define sol 
  (solve (begin (assert a)
                (assert (= x 1))
                (assert (= y 2)))))
(sat? sol)
(evaluate (list 4 5 x) sol)
(define v (vector a))
(evaluate v sol)
(code:line (eq? v (evaluate v sol)) (code:comment "evaluation produces a new vector"))
(evaluate (+ x y) sol)
(evaluate (and a b) sol) 
]
}

@(kill-evaluator rosette-eval)
 