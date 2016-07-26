#lang scribble/manual

@(require (for-label 
           rosette/solver/solver rosette/solver/solution 
           (only-in rosette/query/debug debug)
           rosette/solver/smt/z3 
           rosette/base/form/define rosette/query/query rosette/query/core
           rosette/base/core/term (only-in rosette/base/base bv?)
           (only-in rosette/base/core/safe assert) 
           racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox racket/runtime-path
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "solvers-log")))

@title[#:tag "sec:solvers-and-solutions"]{Solvers and Solutions}

@declare-exporting[rosette/query/core
                   rosette/query/eval
                   rosette/solver/solver
                   rosette/solver/solution
                   rosette/solver/smt/z3                   
                   #:use-sources 
                   (rosette/query/finitize
                    rosette/query/eval
                    rosette/solver/solver
                    rosette/solver/solution
                    rosette/solver/smt/z3)]

A @deftech{solver} is an automatic reasoning engine, used to answer 
@seclink["sec:queries"]{queries} about Rosette programs.  The result of
a solver invocation is a @deftech{solution}, containing either 
a @tech{binding} of symbolic constants to concrete values, or 
an @tech[#:key "MUC"]{unsatisfiable core}. 
Solvers and solutions may not be symbolic.  Two solvers (resp. solutions) are @racket[eq?]/@racket[equal?] 
if they refer to the same object.

@section{The Solver Interface}

@defparam[current-solver solver solver?]{
  The @racket[current-solver] parameter holds the solver object used for 
  answering solver-aided queries.  Rosette's default solver is @racket[z3], although
  new (SMT) solvers can be added well.  Rosette will work with any solver that implements the
  @racket[gen:solver] generic interface.
  @examples[#:eval rosette-eval
   (current-solver)]
}

@defthing[gen:solver solver?]{
  A @hyperlink["https://docs.racket-lang.org/reference/struct-generics.html"]{generic interface}
  that specifies the procedures provided by a solver.  These include
  @racket[solver-assert],
  @racket[solver-clear],
  @racket[solver-minimize],
  @racket[solver-maximize],
  @racket[solver-check],
  @racket[solver-debug], and
  @racket[solver-shutdown].
  Solvers are stateful.  Each solver contains the constraints that have been added to it
  via @racket[solver-assert], and the numeric objective terms that have been added to it
  via @racket[solver-minimize] and @racket[solver-maximize]. 
}

@defproc[(solver? [v any/c]) boolean?]{
Returns true if @racket[v] is a concrete value that implements the @racket[gen:solver] interface.}

@defproc[(solver-assert [solver solver?] [constraints (listof boolean?)]) void?]{
Adds the given constraints to the given solver. These constraints take the form of boolean terms
to be satisfied by subsequent calls to @racket[solver-check].}                                                                                   

@defproc[(solver-clear [solver solver?]) void?]{
Clears all constraints from the given solver.}

@defproc*[([(solver-minimize [solver solver?] [objs (listof (or/c integer? real? bv?))]) void?]
           [(solver-maximize [solver solver?] [objs (listof (or/c integer? real? bv?))]) void?])]{
Adds the given optimization objectives to the given solver. These objectives take the form of
numeric terms whose value is to be minimized or maximized by subsequent calls to @racket[solver-check],
while satisfying all the boolean terms asserted via @racket[solver-assert].}  

@defproc[(solver-check [solver solver?]) solution?]{
Searches for a binding from symbolic constants to concrete values that satisfies the 
constraints (boolean terms) added to the solver via @racket[solver-assert].
If such a binding---or, a @racket[model]---exists, 
it is returned in the form of a satisfiable (@racket[sat?]) solution, which optimizes
the objective terms added to the solver via @racket[solver-minimize] and @racket[solver-maximize].
Otherwise, an unsatisfiable (@racket[unsat?]) solution is returned, but without 
computing an unsatisfiable @racket[core] (i.e., calling @racket[core] on the
resulting solution produces @racket[#f]).
}

@defproc[(solver-debug [solver solver?]) solution?]{
Searches for an unsatisfiable core of the constraints (boolean terms)
added to the solver via @racket[solver-assert] @emph{after} the most recent call to 
@racket[clear] or @racket[solver-check] (if any).
If the constraints are satisfiable, or the given solver does 
not support core extraction, an error is thrown.  Otherwise, the result is an 
@racket[unsat?] solution with a unsatisfiable @racket[core], expressed as a
list of boolean terms.   
}

@defproc[(solver-shutdown [solver solver?]) void?]{
Terminates the current solving process (if any), 
clears all added constraints, and releases all system resources associated 
with the given solver instance.  The solver must be able to reacquire these resources 
if needed.  That is, the solver should behave as though its state was merely cleared
(via @racket[solver-clear]) after a shutdown call.  
}

@defproc[(z3) solver?]{
Returns a @racket[solver?] wrapper for the @hyperlink["https://github.com/Z3Prover/z3/"]{Z3} solver from Microsoft Research.}

@section{Solutions}

A solution to a set of formulas may be satisfiable (@racket[sat?]), unsatisfiable  (@racket[unsat?]),
or unknown (@racket[unknown?]). 
A satisfiable solution can be used as a procedure:  when applied to a bound symbolic constant, it returns 
a concrete value for that constant; when applied to any other value, it returns 
the value itself. 
The solver returns an @racket[unknown?] solution if it cannot determine whether
the given constraints are satisfiable or not.

A solution supports the following operations:

@defproc[(solution? [value any/c]) boolean?]{
Returns true if the given @racket[value] is a solution.}

@defproc[(sat? [value any/c]) boolean?]{
Returns true if the given @racket[value] is a satisfiable solution.}

@defproc[(unsat? [value any/c]) boolean?]{
Returns true if the given @racket[value] is an unsatisfiable solution.}

@defproc[(unknown? [value any/c]) boolean?]{
Returns true if the given @racket[value] is an unknown solution.}

@defproc*[([(sat) sat?]
           [(sat [binding (hash/c constant? any/c #:immutable #t)]) sat?])]{
Returns a satisfiable solution that holds the given binding from symbolic 
constants to values, or that holds the empty binding.  The provided hash must
bind every symbolic constant in its keyset to a concrete value of the same type.
}

@defproc*[([(unsat) unsat?]
           [(unsat [constraints (listof boolean?)]) unsat?])]{
Returns an unsatisfiable solution.  The @racket[constraints] list, if provided, 
consist of boolean values that are collectively unsatisfiable.  If no constraints
are provided, applying @racket[core] to the resulting solution produces @racket[#f],   
indicating that there is no satisfying solution but
core extraction was not performed.  (Core extraction is an expensive 
operation that is not supported by all solvers; those that do support it 
usually don't compute a core unless explicitly asked for one via @racket[solver-debug].)}

@defproc[(unknown) unknown?]{
Returns an unknown solution.}

@defproc[(model [solution sat?]) (hash/c constant? any/c #:immutable #t)]{
Returns the binding stored in the given satisfiable solution.  The binding is an immutable
hashmap from symbolic constants to values.  
}

@defproc[(core [solution unsat?]) (or/c (listof (and/c constant? boolean?)) #f)]{
Returns the unsatisfiable core stored in the given satisfiable solution.  If the solution is 
@racket[unsat?] and a core was computed, the result is a list of boolean values that 
are collectively unsatisfiable.  Otherwise, the result is @racket[#f]. 
}

@defproc[(evaluate [v any/c] [solution sat?]) any/c]{
Given a Rosette value and a satisfiable solution, @racket[evaluate] produces a 
new value obtained by replacing every symbolic constant @var[c] in @racket[v] 
with @racket[(solution #, @var[c])] and simplifying the result.
@examples[#:eval rosette-eval                
(define-symbolic a b boolean?)
(define-symbolic x y integer?)
(define sol 
  (solve (begin (assert a)
                (assert (= x 1))
                (assert (= y 2)))))
(sat? sol)
(evaluate (list 4 5 x) sol)
(define vec (vector a))
(evaluate vec sol)
(code:line (eq? vec (evaluate vec sol)) (code:comment "evaluation produces a new vector"))
(evaluate (+ x y) sol)
(evaluate (and a b) sol) 
]}

@(kill-evaluator rosette-eval)
 