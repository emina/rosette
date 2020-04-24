#lang scribble/manual

@(require scribble/core scribble/html-properties
          scribble/bnf scribble/example scriblib/footnote
          (for-label (except-in racket list-set) errortrace
                     rosette/base/core/term
                     rosette/base/form/define
                     rosette/query/form
                     (only-in rosette/query/debug debug)
                     rosette/base/core/union
                     (only-in rosette unsat model evaluate sat? unsat? clear-asserts!)
                     (only-in rosette/query/finitize current-bitwidth)
                     (only-in rosette/base/base bitvector)
                     (only-in rosette/base/core/safe assert)
                     (only-in rosette/base/core/forall for/all)
                     rackunit)
          racket/runtime-path
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "error-tracer-log") #f 'rosette))

@title[#:tag "ch:error-tracing"]{Debugging}

Bugs in Rosette programs often manifest as runtime
exceptions. For example, calling a procedure with too few
arguments will cause a runtime exception in Rosette, just as
it would in Racket. But unlike Racket, Rosette treats
exceptions as assertion failures: it catches the exception,
adds the (path) condition under which the exception is
thrown to the @tech{assertion store}, and proceeds with
symbolic evaluation. This treatment of exceptions
ensures that the program's
@seclink["ch:syntactic-forms:rosette"]{solver-aided queries}
correctly return a @racket[sat?] or @racket[unsat?]
solution, but it can also make solver-aided code tricky to
debug. This chapter describes common problems that are due to
intercepted exceptions, how to test for them, and how to
find them with the @code{symtrace} tool for error tracing.


@section[#:tag "sec:errors-in-rosette"]{Common Bugs in Solver-Aided Code}

Rosette intercepts exceptions in two places: within
solver-aided queries and within conditional expressions.
When converted to assertion failures, these exceptions can
lead to unexpected query results, as well as subtle logical
errors with no obvious manifestation. We illustrate both
kinds of problems next and show how to test for them.


@subsection[#:tag "sec:errors-under-queries"]{Bugs Due to Exceptions in Solver-Aided Queries}

When an exception is intercepted within a solver-aided
query, the query will often produce an unexpected result:
a model when we expect @racket[unsat], and vice versa. 

As an example, consider the following verification query,
which tries to prove that the sum of a list of integers
remains the same when all zeros are removed from the list:

@examples[#:label #f #:eval rosette-eval
(define-symbolic xs integer? [4])
(code:line (define (sum xs) (foldl + xs)) (code:comment "bug: missing 0 after +"))
(verify (assert (= (sum xs) (sum (filter-not zero? xs)))))
]

Because we expect this property to hold, we expect the query
to return @racket[unsat]. Instead, it returns the empty model.

To see why, note that the @racket[sum] procedure contains a
simple bug. We forgot to provide the initial value of 0 to
@racket[foldl], so @racket[foldl] is called with too few
arguments. This omission will cause every call to
@racket[sum] to raise an exception, including 
@racket[(sum xs)] in the body of our query. Rosette
intercepts this exception and adds @racket[#f] to the
query's @tech{assertion store} because the exception
happens unconditionally (on all paths).
This false assertion then causes the query to return a
trivial counterexample, @racket[(model)], indicating that
@emph{any} binding of @racket[xs] to concrete integers leads
to an error.

As another example, consider the following synthesis query
involving @racket[sum]:

@examples[#:label #f #:eval rosette-eval
(define-symbolic opt boolean?)
(synthesize
 #:forall xs
 #:guarantee (assert (= (sum xs) (apply (if opt + -) xs))))
]

Here, the expected result is a model that binds @racket[opt]
to the value @racket[#t], and this is the outcome we see
once we fix the bug in @racket[sum]. The bug, however,
causes the @racket[#:guarantee] expression to fail
unconditionally. The query then intercepts the exception and
returns @racket[(unsat)] to indicate that no choice of
@racket[opt] can satisfy the specification.

In addition to unexpected results, exceptions intercepted in
queries can also lead to more subtle errors, with no obvious
manifestation. For example, consider the following query
that verifies another simple property of @racket[sum]---if
@racket[sum] returns a positive integer, then at least one
of its arguments must have been positive.

@examples[#:label #f #:eval rosette-eval
(verify
 #:assume (assert (positive? (sum xs)))
 #:guarantee (assert (ormap positive? xs)))
]

This query returns @racket[(unsat)], as expected, despite the
bug in @racket[sum]. To see why, recall that the verifier
returns a counterexample when it can find an input that
satisfies the @racket[#:assume]d assertions and violates the
@racket[#:guarantee]d assertions. But there is no input that
satisfies the assumptions in our query: as before, the call
to @racket[(sum xs)] throws an unconditional exception that
is intercepted and treated as @racket[(assert #f)]. This
false assumption leads to a vacuous proof of correctness,
and the verifier returns @racket[(unsat)].

The bugs due to query exceptions can generally be found
through testing. To guard against such bugs, a test suite
should check that queries produce expected results on small
inputs, and it should also check for unexpected exceptions
in all parts of the query's body. When possible, it is also
good practice to test all solver-aided code against concrete
inputs and outputs. Here is an example test suite for our
last query that includes all of these checks:

@examples[#:label #f #:eval rosette-eval
(eval:no-prompt
(require rackunit)

(define (assumed xs)
  (assert (positive? (sum xs))))

(define (guaranteed xs)
  (assert (ormap positive? xs)))

(define (query xs)
  (verify #:assume (assumed xs)
          #:guarantee (guaranteed xs)))
  
(define example-tests 
  (test-suite
   "An example suite for a sum query."
   #:before clear-asserts!
   #:after  clear-asserts!

   (test-case
    "Test sum with concrete values."
    (check = (sum '()) 0)
    (check = (sum '(-1)) -1)
    (check = (sum '(-2 2)) 0)
    (check = (sum '(-1 0 3)) 2))
     
   (test-case
    "Test query parts for exceptions."
    (check-not-exn (thunk (assumed xs)))
    (check-not-exn (thunk (guaranteed xs))))
  
   (test-case
    "Test query outcome."
    (check-pred unsat? (query xs))
    ))))

(eval:alts (run-test example-tests) '(\#<test-error> \#<test-failure> \#<test-success>))
]

All but the last test in this suite fail when invoked on the
buggy @racket[sum], and they all pass once the bug is fixed.


@subsection[#:tag "sec:errors-under-symbolic-eval"]{Bugs Due to Exceptions in Conditionals}

As we saw above, basic tests can easily uncover problems
caused by exceptions that are raised unconditionally, on all
paths. This is not surprising since such problems are also
easy to discover in concrete code---they correspond to
obvious bugs that cause an immediate crash on every input.
Catching bugs that raise exceptions only on some paths is
trickier, in both concrete and solver-aided code, as our next
example shows.

Consider the following buggy version of @racket[sum]:
@examples[#:label #f #:eval rosette-eval
(define (sum xs)
  (cond
    [(null? xs) 0]
    [(null? (cdr xs)) (car xs)] 
    [(andmap (curry = (car xs)) (cdr xs))  
     (* (length xs) (cdr xs))] (code:comment "bug: cdr should be car")
    [else (apply + xs)]))
]

This version of @racket[sum] implements three simple
optimizations. It returns 0 when given an empty list; @code{
 xs[0]} when given a list of length 1; and @code{|xs| * xs[0]}
when given a list of identical elements. This last
optimization is buggy (it uses @racket[cdr] when it should
have used @racket[car]), and any execution path that goes
through it will end with an exception.

Our test suite from the
@seclink["sec:errors-under-queries"]{previous section} will
not uncover this bug. If we run the tests against the new
@racket[sum], all the checks pass:

@examples[#:label #f #:eval rosette-eval
(eval:alts (run-test example-tests)
           '(\#<test-success> \#<test-success> \#<test-success>))
]

One way to detect bugs of this kind is to run a "unit
verification query" for each key procedure in the program,
searching for assertion failures where none are expected:

@examples[#:label #f #:eval rosette-eval
(test-case
 "Test sum for any failures."
 (check-pred unsat? (verify (sum xs))))
]

But writing such queries is not always possible, or
foolproof, for large programs. So, in addition to testing,
we recommend debugging all important queries with 
@tech[#:key "error tracer"]{error tracing}. 


@section[#:tag "sec:error-tracer"]{Error Tracer}

To help debug solver-aided code, Rosette provides an
@deftech[#:key "error tracer"]{error tracer} that tracks and
displays all exceptions raised during symbolic evaluation.
Some of these exceptions are due to bugs and some are
intentional, especially in the context of synthesis queries.
It is not possible to automatically distinguish between
these two, so the error tracer leaves that task to the
programmer.


@bold{Running the error tracer} has the same limitations as
@racketmodname[errortrace] and the @seclink["sec:sympro"]{
 symbolic profiler}: before running it, throw away any
@filepath{.zo} versions of your program (i.e., delete any
@filepath{compiled} folders in your code's folder hierarchy).

Then invoke the error tracer on a program file @nonterm{prog}
using @exec{raco}:

@commandline{raco symtrace @nonterm{prog}}

After executing @nonterm{prog}, the error tracer will output
all the exceptions that Rosette intercepted. For instance,
here is the output from the error tracer when running our
last query on the buggy @racket[sum] example from the
@seclink["sec:errors-under-symbolic-eval"]{previous
 section}:

@examples[#:label #f #:eval rosette-eval
(verify
 #:assume (assert (positive? (sum xs)))
 #:guarantee (assert (ormap positive? xs)))
]

@verbatim|{
--------------------------------------------------------------------------------
exn: $*: expected real? arguments
  arguments: (list (list xs@1 xs@2 xs@3))
  context...:
   ~/rosette/base/core/real.rkt:140:0: safe-apply-2
   ~/rosette/lib/trace/tool.rkt:122:5
   /Racket v7.6/collects/racket/private/more-scheme.rkt:265:2: call-with-exception-handler
   ...

at: ex.rkt line 10 column 5
(* (length xs) (cdr xs))

call stack: 
ex.rkt:10:5 *
ex.rkt:15:31 sum
}|

The output shows a list of exceptions that Rosette
intercepted; here, there is one exception caused by our bug.
For each exception, there are three parts:

@itemlist[
  @item{The exception message,}
  @item{The expression that causes the exception, and}
  @item{The call stack when the exception occurs.}
]

@subsection[#:tag "sec:symtrace:opts"]{Options and Caveats}

By default, the error tracer instruments only code that is
within a module with either @tt{rosette} or @tt{
 rosette/safe} as its initial path. This default is inherited
from the symbolic profiler, and it means that only files
beginning with @tt{#lang rosette} or @tt{#lang rosette/safe}
will be instrumented. The shown call stacks and expressions
will not include non-instrumented files. To instrument
@emph{all} code, use the @DFlag{racket} flag described
below.

The @exec{raco symtrace @nonterm{prog}} command accepts the following command-line flags:

@itemlist[
 @item{@DFlag{stream} --- stream the intercepted exception
  while executing the program, rather than producing output
  only once the program completes. This option is useful for
  programs that do not terminate, or take a very long time to
  run. Note however that the output can interleave with
  regular output from program execution, which might not be
  desirable.}

 @item{@DFlag{module} @nonterm{module-name} --- run the
  specified @nonterm{module-name} submodule of @nonterm{prog}
  (defaults to the @tt{main} submodule).}

 @item{@DFlag{racket} --- instrument code in any module, not
  just those derived from Rosette.}

 @item{@DFlag{solver} --- do not show exceptions raised on
  infeasible paths, using the solver to decide if paths are
  feasible. This option can lead to significant slow downs.}

 @item{@DFlag{assert} --- do not show exceptions due to
  assertion errors, which are usually expected exceptions.}

 @item{@DFlag{context} @nonterm{context-length} --- set the
  length of the exception context (defaults to 3).}
  ]

@section{Walkthrough: Tracing Errors in Rosette}

To illustrate a typical error tracing process, consider
verifying the following buggy implementation of the
@link["https://en.wikipedia.org/wiki/Quickselect"]{
 quickselect algorithm}.

@examples[#:label #f #:eval rosette-eval
(define (select xs n)
  (cond
    [(empty? xs) (assert #f "unexpected empty list")]
    [else (define pivot (first xs))
          (define non-pivot (rest xs))
          (define <pivot (filter (λ (x) (< x pivot)) non-pivot))
          (define >=pivot (filter (λ (x) (>= x pivot)) non-pivot))
          (define len< (length <pivot))
          (cond
            [(= n len<) pivot]
            [(< n len<) (select <pivot)] (code:comment "bug: should be (select <pivot n)")
            [else (select >=pivot (- n len< 1))])]))

(verify
 #:assume (assert (and (<= 0 n (sub1 (length xs)))
                       (= k (select xs n))))
 #:guarantee (assert (= k (list-ref (sort xs <) n))))
]

As before, the verification query succeeds despite the bug.
But unlike before, the bug is much harder to detect. So we
run the error tracer on it and obtain the following output:

@verbatim|{
--------------------------------------------------------------------------------
exn: select: arity mismatch;
 the expected number of arguments does not match the given number
  expected: 2
  given: 1
  arguments...:
   {[(&& (< xs@1 xs@0) (< xs@2 xs@0) (< ...)) (xs@1 xs@2 xs@3)] ...
  context...:
   ~/rosette/lib/trace/tool.rkt:121:5
   /Racket v7.6/collects/racket/private/more-scheme.rkt:265:2: call-with-exception-handler
   ~/rosette/lib/trace/compile.rkt:267:11
   ...

at: ex.rkt line 16 column 24
(select <pivot)

call stack:
ex.rkt:16:24 select
ex.rkt:21:28 select

--------------------------------------------------------------------------------
exn: assert: unexpected empty list
  context...:
   ~/rosette/lib/trace/compile.rkt:267:11
   /Racket v7.6/collects/racket/private/more-scheme.rkt:265:2: call-with-exception-handler
   .../more-scheme.rkt:261:28
   ...

at: ex.rkt line 8 column 18
(assert #f "unexpected empty list")

call stack:
ex.rkt:17:18 select
ex.rkt:21:28 select

--------------------------------------------------------------------------------

[6 exceptions similar to above exceptions skipped]

--------------------------------------------------------------------------------
exn: assert: both branches infeasible
  context...:
   .../more-scheme.rkt:261:28
   ~/rosette/base/form/control.rkt:33:0: branch-and-merge
   ~/rosette/lib/trace/compile.rkt:267:11
   ...

at: ex.rkt line 14 column 10
(cond
  [(= n len<) pivot]
  [(< n len<) (select <pivot)]
  [else (select-buggy >=pivot (- n len< 1))])

call stack:
ex.rkt:17:18 select
ex.rkt:17:18 select
ex.rkt:17:18 select
ex.rkt:21:28 select
}|

The output from the error tracer includes 9 exceptions. Four
are arity mismatch exceptions that are due to the bug, and
the rest are benign assertion failures that cannot happen in
any concrete execution.

Because benign assertion failures are so common, the error
tracer provides an option to suppress them from the output
via the @seclink["sec:symtrace:opts"]{@DFlag{assert}}
flag. With the flag enabled, the output contains only the
four arity mismatch exceptions.

@define-footnote[pc-note make-pc-note]

Some assertion failures are bugs, however, so aggressive
filtering with @DFlag{assert} can end up hiding true
positives. For this reason, the error tracer also includes
a more conservative filtering option, @DFlag{solver},
that will never miss true positives, at
the cost of showing more false alarms. The option suppress
exceptions that are raised on paths with
@seclink["sec:state-reflection"]{ infeasible path
 conditions}.@pc-note{Exceptions with infeasible path
 conditions are guaranteed to be unreachable, so no true
 positive are missed. But exceptions with feasible path
 conditions may still be unreachable due to the way that
 Rosette represents symbolic state, so some false positives
 may be included in the output.} by using the solver.
Note that due to the use of the solver,
the error tracing could be significantly slower.
In our example, the @DFlag{solver} flag suppresses all the
assertion failures as well as one of the arity mismatch
errors.

Lastly, it is possible to combine
both @DFlag{solver} and @DFlag{assert} together.
For our example, the output from the these flags
would not be different from the output from @DFlag{solver} alone.
However, doing so could speed up error tracing
(relative to the one with @DFlag{solver}), since the error tracer
can prune calls to the solver.

@make-pc-note[]