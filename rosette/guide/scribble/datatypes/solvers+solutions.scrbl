#lang scribble/manual

@(require (for-label 
           rosette/solver/solver rosette/solver/solution 
           rosette/solver/smt/z3 rosette/solver/smt/cvc4
           rosette/solver/smt/boolector rosette/solver/smt/yices 
           rosette/base/form/define rosette/query/query 
           rosette/base/core/term (only-in rosette/base/base bv?)
           (only-in rosette/base/base assert) 
           racket)
          scribble/core scribble/html-properties scribble/examples racket/sandbox racket/runtime-path
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "solvers-log")))

@title[#:tag "sec:solvers-and-solutions"]{Solvers and Solutions}

@declare-exporting[rosette/query/query
                   rosette/solver/solver
                   rosette/solver/solution
                   rosette/solver/smt/z3
                   rosette/solver/smt/cvc4
                   rosette/solver/smt/yices
                   rosette/solver/smt/boolector
                   #:use-sources 
                   (rosette/query/finitize
                    rosette/query/query
                    rosette/solver/solver
                    rosette/solver/solution
                    rosette/solver/smt/z3
                    rosette/solver/smt/cvc4
                    rosette/solver/smt/yices
                    rosette/solver/smt/boolector)]

A @deftech{solver} is an automatic reasoning engine, used to answer 
@seclink["sec:queries"]{queries} about Rosette programs.  The result of
a solver invocation is a @deftech{solution}, containing either 
a @tech{binding} of symbolic constants to concrete values, or 
an @link["https://en.wikipedia.org/wiki/Unsatisfiable_core"]{unsatisfiable core}. 
Solvers and solutions may not be symbolic.  Two solvers (resp. solutions) are @racket[eq?]/@racket[equal?] 
if they refer to the same object.

@section{The Solver Interface}

A solver contains a stack of assertions (boolean terms) to satisfy and a set of objectives (numeric terms) to optimize. 
The assertion stack is partitioned into levels, with each level containing
a set of assertions. The bottom (0) assertion level cannot be removed, but more levels
can be created and removed using the @racket[solver-push] and @racket[solver-pop] procedures.
The @racket[solver-assert] procedure adds assertions to the top level of the assertion stack, while 
the @racket[solver-minimize] and @racket[solver-maximize] procedures add new terms to the current set of optimization objectives.
The @racket[solver-check] procedure checks the satisfiability of all assertions in the assertion stack,
optimizing the resulting solution (if any) with respect to the provided objectives. 

@defparam[current-solver solver solver?]{
  The @racket[current-solver] parameter holds the solver object used for 
  answering solver-aided queries.  Rosette's default solver is @racket[z3], although
  new (SMT) solvers can be added as well.  Rosette will work with any solver that implements the
  @racket[gen:solver] generic interface.
  @examples[#:eval rosette-eval
   (current-solver)]
}

@defthing[gen:solver solver?]{
  A @hyperlink["https://docs.racket-lang.org/reference/struct-generics.html"]{generic interface}
  that specifies the procedures provided by a solver.  These include
  @racket[solver-assert],
  @racket[solver-push],
  @racket[solver-pop],
  @racket[solver-clear],
  @racket[solver-minimize],
  @racket[solver-maximize],
  @racket[solver-check],
  @racket[solver-debug],
  @racket[solver-shutdown], and
  @racket[solver-features].
  A solver may support a subset of this interface, which loosely follows
  the @hyperlink["http://smtlib.cs.uiowa.edu/papers/smt-lib-reference-v2.5-r2015-06-28.pdf"]{SMTLib solver interface}.

 
}

@defproc[(solver? [v any/c]) boolean?]{
Returns true if @racket[v] is a concrete value that implements the @racket[gen:solver] interface.}

@defproc[(solver-assert [solver solver?] [constraints (listof boolean?)]) void?]{
Takes as input a list of boolean terms or values and
adds them to the current (top) level in the assertion stack.}                                                                                   
@defproc[(solver-push [solver solver?]) void?]{
Pushes a new level onto the solver's assertion stack.  Subsequent calls to
@racket[solver-assert] will add assertions to this level.}

@defproc[(solver-pop [solver solver?] [levels integer?]) void?]{
Pops the given number of levels off the solver's assertion stack,
removing all the assertions at the popped levels. The number of @racket[levels] to
pop must be a positive integer that is no greater than the number of preceding
calls to @racket[solver-push].}

@defproc[(solver-clear [solver solver?]) void?]{
Clears the assertion stack of all levels and all assertions,
and removes all objectives from the current set of objectives to optimize.}

@defproc*[([(solver-minimize [solver solver?] [objs (listof (or/c integer? real? bv?))]) void?]
           [(solver-maximize [solver solver?] [objs (listof (or/c integer? real? bv?))]) void?])]{
Adds the given optimization objectives to the given solver. These objectives take the form of
numeric terms whose value is to be minimized or maximized by subsequent calls to @racket[solver-check],
while satisfying all the boolean terms asserted via @racket[solver-assert].}  

@defproc[(solver-check [solver solver?]) solution?]{
Searches for a binding from symbolic constants to concrete values that satisfies all  
constraints (boolean terms) added to the solver via @racket[solver-assert].
If such a binding---or, a @racket[model]---exists, 
it is returned in the form of a satisfiable (@racket[sat?]) solution, which optimizes
the objective terms added to the solver via @racket[solver-minimize] and @racket[solver-maximize].
Otherwise, an unsatisfiable (@racket[unsat?]) solution is returned, but without 
computing an unsatisfiable @racket[core] (i.e., calling @racket[core] on the
resulting solution produces @racket[#f]).
}

@defproc[(solver-debug [solver solver?]) solution?]{
Searches for an unsatisfiable core of all constraints (boolean terms)
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

@defproc[(solver-features [solver solver?]) (listof symbol?)]{
Returns the list of features supported by the solver.
The possible features, which correspond roughly to SMTLib @emph{logics},
extended with some additional options, are:

@itemize[
@item{@racket['qf_bv] (quantifier-free fixed-width bitvectors)}
@item{@racket['qf_uf] (quantifier-free uninterpreted functions and equality)}
@item{@racket['qf_lia] (quantifier-free linear integer arithmetic)}
@item{@racket['qf_nia] (quantifier-free non-linear integer arithmetic)}
@item{@racket['qf_lra] (quantifier-free linear real arithmetic)}
@item{@racket['qf_nra] (quantifier-free non-linear real arithmetic)}
@item{@racket['quantifiers] (quantified versions of the supported quantifier-free logics)}
@item{@racket['optimize] (support for objective function optimization)}
@item{@racket['unsat-cores] (unsatisfiable core generation)}
]

}

@defproc[(solver-options [solver solver?]) (hash/c symbol? any/c)]{
Returns the options the given solver is configured with
(as specified by the @racket[#:options] argument to solver constructors).
}

@defparam[output-smt on? (or/c boolean? path-string? output-port?)]{
  Enables verbose output of generated SMT constraints.

  When the @racket[output-smt] parameter is @racket[#t] or a @racket[path-string?],
  Rosette will log the SMT encoding of all solver queries to temporary files.
  A new temporary file is created for each solver process Rosette spawns.
  Note that a single solver-aided query may spawn multiple solver processes,
  and Rosette may reuse a solver process across several solver-aided queries.
  When @racket[output-smt] is @racket[#t], the temporary files are created
  in the system's temporary directory; otherwise,
  the temporary files are created in the given path (which must be a directory).
  The path to each temporary file is printed to @racket[current-error-port]
  when it is first created.

  When the @racket[output-smt] parameter is an @racket[output-port?],
  Rosette will log the SMT encoding to that output port.
  For example, setting @racket[output-smt] to @racket[(current-output-port)]
  will print the SMT encoding to standard output.
  All solvers will log to the same output port,
  so several separate encodings may be interleaved when multiple solvers are in use.

  Default value is @racket[#f].
}


@section{Supported Solvers}

Rosette supports several SMT solvers.
The @racket[current-solver] parameter controls the solver used for answering solver-aided queries.
Each supported solver is contained in a separate module
(e.g., @racketmodname[rosette/solver/smt/z3]),
which exports a constructor (e.g., @racket[z3])
to create a new solver instance.

@subsection{Z3}

@defmodule[rosette/solver/smt/z3 #:no-declare]

@defproc*[([(z3 [#:path path (or/c path-string? #f) #f]
                [#:logic logic (or/c symbol? #f) #f]
                [#:options options (hash/c symbol? any/c) (hash)]) solver?]
           [(z3? [v any/c]) boolean?])]{
                                        
Returns a @racket[solver?] wrapper for the @hyperlink["https://github.com/Z3Prover/z3/"]{Z3} solver from Microsoft Research.
Rosette automatically installs a version of Z3;
the optional @racket[path] argument overrides this version with a path to a new Z3 binary.

The optional @racket[logic] argument specifies an SMT logic for the solver to use (e.g., @racket['QF_BV]).
Specifying a logic can improve solving performance, but Rosette makes no effort to check that
emitted constraints fall within the chosen logic. The default is @racket[#f],
which uses Z3's default logic.

The @racket[options] argument provides additional options that are sent to Z3
via the @tt{set-option} SMT command.
For example, setting @racket[options] to @racket[(hash ':smt.relevancy 0)]
will send the command @tt{(set-option :smt.relevancy 0)} to Z3 prior to solving.
}


@subsection{CVC4}

@defmodule[rosette/solver/smt/cvc4 #:no-declare]

@defproc*[([(cvc4 [#:path path (or/c path-string? #f) #f]
                  [#:logic logic (or/c symbol? #f) #f]
                  [#:options options (hash/c symbol? any/c) (hash)]) solver?]
           [(cvc4? [v any/c]) boolean?])]{
                                          
Returns a @racket[solver?] wrapper for the @hyperlink["http://cvc4.cs.stanford.edu/web/"]{CVC4} solver from NYU and UIowa.

To use this solver, download and install CVC4 (version 1.8 or later),
and either add the @tt{cvc4} executable to your @tt{PATH}
or pass the path to the executable as the optional @racket[path] argument.

The optional @racket[logic] argument specifies an SMT logic for the solver to use (e.g., @racket['QF_BV]).
Specifying a logic can improve solving performance, but Rosette makes no effort to check that
emitted constraints fall within the chosen logic. The default is @racket[#f],
which uses CVC4's default logic.

The @racket[options] argument provides additional options that are sent to CVC4
via the @tt{set-option} SMT command.
For example, setting @racket[options] to @racket[(hash ':bv-propagate 'true)]
will send the command @tt{(set-option :bv-propagate true)} to CVC4 prior to solving.
}

@defproc[(cvc4-available?) boolean?]{
Returns true if the CVC4 solver is available for use (i.e., Rosette can locate a @tt{cvc4} binary).
If this returns @racket[#f], @racket[(cvc4)] will not succeed
without its optional @racket[path] argument.}


@subsection{Boolector}

@defmodule[rosette/solver/smt/boolector #:no-declare]

@defproc*[([(boolector [#:path path (or/c path-string? #f) #f]
                       [#:logic logic (or/c symbol? #f) #f]
                       [#:options options (hash/c symbol? any/c) (hash)]) solver?]
           [(boolector? [v any/c]) boolean?])]{
                                               
Returns a @racket[solver?] wrapper for the @hyperlink["http://fmv.jku.at/boolector/"]{Boolector} solver from JKU.

To use this solver, download and install Boolector (version 2.4.1 or later),
and either add the @tt{boolector} executable to your @tt{PATH}
or pass the path to the executable as the optional @racket[path] argument.

The optional @racket[logic] argument specifies an SMT logic for the solver to use (e.g., @racket['QF_BV]).
Specifying a logic can improve solving performance, but Rosette makes no effort to check that
emitted constraints fall within the chosen logic. The default is @racket[#f],
which uses Boolector's default logic.

The @racket[options] argument provides additional options that are sent to Boolector
via the @tt{set-option} SMT command.
For example, setting @racket[options] to @racket[(hash ':seed 5)]
will send the command @tt{(set-option :seed 5)} to Boolector prior to solving.
}

@defproc[(boolector-available?) boolean?]{
Returns true if the Boolector solver is available for use (i.e., Rosette can locate a @tt{boolector} binary).
If this returns @racket[#f], @racket[(boolector)] will not succeed
without its optional @racket[path] argument.}


@subsection{Yices}

@defmodule[rosette/solver/smt/yices #:no-declare]

@defproc*[([(yices [#:path path (or/c path-string? #f) #f]
                   [#:logic logic symbol? 'ALL]
                   [#:options options (hash/c symbol? any/c) (hash)]) solver?]
           [(yices? [v any/c]) boolean?])]{
                                           
Returns a @racket[solver?] wrapper for the @hyperlink["http://yices.csl.sri.com/"]{Yices} solver from SRI.

To use this solver, download and install Yices (version 2.6.0 or later),
and either add the @tt{yices-smt2} executable to your @tt{PATH}
or pass the path to the executable as the optional @racket[path] argument.

The optional @racket[logic] argument specifies an SMT logic for the solver to use (e.g., @racket['QF_BV]).
Specifying a logic can improve solving performance, but Rosette makes no effort to check that
emitted constraints fall within the chosen logic. The default is @racket['ALL].

The @racket[options] argument provides additional options that are sent to Yices
via the @tt{set-option} SMT command.
For example, setting @racket[options] to @racket[(hash ':random-seed 5)]
will send the command @tt{(set-option :random-seed 5)} to Yices prior to solving.
}

@defproc[(yices-available?) boolean?]{
Returns true if the Yices solver is available for use (i.e., Rosette can locate a @tt{yices-smt2} binary).
If this returns @racket[#f], @racket[(yices)] will not succeed
without its optional @racket[path] argument.}

@section{Solutions}

A solution to a set of formulas may be satisfiable (@racket[sat?]), unsatisfiable  (@racket[unsat?]),
or unknown (@racket[unknown?]). 
A satisfiable solution can be used as a procedure:  when applied to a bound symbolic constant, it returns 
a concrete value for that constant; when applied to any other value, it returns 
the value itself. 
The solver returns an @racket[unknown?] solution if it cannot determine whether
the given constraints are satisfiable or not.

A solution supports the following operations:

@defproc[(solution? [v any/c]) boolean?]{
Returns true if @racket[v] is a solution.}

@defproc[(sat? [v any/c]) boolean?]{
Returns true if @racket[v] is a satisfiable solution.}

@defproc[(unsat? [v any/c]) boolean?]{
Returns true if @racket[v] is an unsatisfiable solution.}

@defproc[(unknown? [v any/c]) boolean?]{
Returns true if @racket[v] is an unknown solution.}

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
do not compute a core unless explicitly asked for one via @racket[solver-debug].)}

@defproc[(unknown) unknown?]{
Returns an unknown solution.}

@defproc[(model [sol sat?]) (hash/c constant? any/c #:immutable #t)]{
Returns the binding stored in the given satisfiable solution.  The binding is an immutable
hashmap from symbolic constants to values.  
}

@defproc[(core [sol unsat?]) (or/c (listof (and/c constant? boolean?)) #f)]{
Returns the unsatisfiable core stored in the given satisfiable solution.  If the solution is 
@racket[unsat?] and a core was computed, the result is a list of boolean values that 
are collectively unsatisfiable.  Otherwise, the result is @racket[#f]. 
}

@defproc[(evaluate [v any/c] [sol sat?]) any/c]{
Given a Rosette value and a satisfiable solution, @racket[evaluate] produces a 
new value obtained by replacing every symbolic constant @var[c] in @racket[v] 
with @racket[(sol #, @var[c])] and simplifying the result.

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
(code:line (eq? vec (evaluate vec sol)) (code:comment "Evaluation produces a new vector."))
(evaluate (+ x y) sol)
(evaluate (and a b) sol) 
]}

@defproc[(complete-solution [sol solution?] [consts (listof constant?)]) solution?]{
                                                                                    
 Given a solution @racket[sol] and a list of symbolic
 constants @racket[consts], returns a solution that is
 complete with respect to the given list. In particular, if
 @racket[sol] is satisfiable, the returned solution is also
 satisfiable, and it extends the @racket[sol] model with
 default bindings for all constants in @racket[consts] that
 are not bound by @racket[sol]. Otherwise, @racket[sol]
 itself is returned.

@examples[#:eval rosette-eval                
(define-symbolic a boolean?)
(define-symbolic x integer?)
(define sol (solve (assert a)))
(code:line sol (code:comment "No binding for x."))
(complete-solution sol (list a x))
(complete-solution (solve (assert #f)) (list a x))
]}

@(kill-evaluator rosette-eval)
 
