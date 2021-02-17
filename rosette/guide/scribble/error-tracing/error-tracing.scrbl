#lang scribble/manual

@(require scribble/core scribble/html-properties
          scribble/bnf scribble/example 
          (for-label (except-in racket list-set) errortrace
                     rosette/base/core/term
                     rosette/base/form/define
                     rosette/query/form
                     rosette/base/core/union
                     (only-in rosette unsat model evaluate sat? unsat? clear-vc! current-bitwidth)
                     (only-in rosette/base/base assume assert vc clear-vc!)
                     rackunit)
          racket/runtime-path
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "error-tracer-log") #f 'rosette))

@(define-runtime-path interface.png "interface.png")
@(define-runtime-path quickselect.png "quickselect.png")

@(rosette-eval '(require (only-in rosette/guide/scribble/util/lifted format-opaque)))

@title[#:tag "ch:error-tracing"]{Debugging}

Bugs in Rosette programs often manifest as runtime
exceptions. For example, calling a procedure with too few
arguments will cause a runtime exception in Rosette, just as
it would in Racket. But unlike Racket, Rosette treats
exceptions as assertion failures: it catches the exception,
updates the @tech{verification condition} to reflect the
failure, and proceeds with symbolic evaluation. This
treatment of exceptions ensures that the program's
@seclink["ch:syntactic-forms:rosette"]{ solver-aided
 queries} correctly return a @racket[sat?] or @racket[unsat?]
solution, but it can also make solver-aided code tricky to
debug. This chapter describes common problems that are due
to intercepted exceptions, how to test for them, and how to
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

@examples[#:eval rosette-eval #:label #f 
(define-symbolic xs integer? #:length 4)
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
query's @tech{verification condition} because the exception
happens unconditionally (on all paths).
This false assertion then causes the query to return a
trivial counterexample, @racket[(model)], indicating that
@emph{any} binding of @racket[xs] to concrete integers leads
to an error.

As another example, consider the following synthesis query
involving @racket[sum]:

@examples[#:eval rosette-eval #:label #f 
(define-symbolic opt boolean?)
(synthesize
 #:forall xs
 #:guarantee (assert (= (sum xs) (apply (if opt + -) xs))))
]

Here, the expected result is a model that binds @racket[opt]
to the value @racket[#t], and this is the outcome we see
once we fix the bug in @racket[sum]. The bug, however,
causes the @racket[#:guarantee] expression to fail
unconditionally. Rosette then intercepts the exception and
returns @racket[(unsat)] to indicate that no choice of
@racket[opt] can satisfy the specification.

Bugs of this kind can be found through testing. A good test
suite should check that queries produce expected results on
small inputs, and that query parts do not throw exceptions.
When possible, it is also good practice to test all
solver-aided code against concrete inputs and outputs. Here
is an example test suite for our first query that includes
all of these checks:

@examples[#:eval rosette-eval #:label #f 
(eval:no-prompt
(require rackunit)

(define (post xs)
  (assert (= (sum xs) (sum (filter-not zero? xs)))))

(define (query xs)
  (verify (post xs)))
   
(define example-tests 
  (test-suite
   "An example suite for a sum query."
   #:before clear-vc!
   #:after  clear-vc!

   (test-case
    "Test sum with concrete values."
    (check = (sum '()) 0)
    (check = (sum '(-1)) -1)
    (check = (sum '(-2 2)) 0)
    (check = (sum '(-1 0 3)) 2))
     
   (test-case
    "Test query post for exceptions."
    (before
     (clear-vc!)
     (check-not-exn (thunk (post xs)))))
  
   (test-case
    "Test query outcome."
    (before
     (clear-vc!)
     (check-pred unsat? (query xs)))))))

(eval:alts
 (run-test example-tests)
 (format-opaque "~a" (run-test example-tests)))
]

All tests in this suite fail when invoked on the
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
@examples[#:eval rosette-eval #:label #f 
(define (sum xs)
  (cond
    [(null? xs) 0]
    [(null? (cdr xs)) (car xs)] 
    [(andmap (curry = (car xs)) (cdr xs))  
     (* (length xs) (cdr xs))] (code:comment "Bug: cdr should be car.")
    [else (apply + xs)]))
]

This version of @racket[sum] implements three simple
optimizations. It returns 0 when given an empty list;
@code{xs[0]} when given a list of length 1; and
@code{|xs| * xs[0]} when given a list of identical elements.
This last optimization is buggy (it uses @racket[cdr] when
it should have used @racket[car]), and any execution path
that goes through it will end with an exception.

Suppose that we want to verify another simple property of
@racket[sum]: if it returns a positive integer, then at least
one element in the argument list must have been positive.

@examples[#:eval rosette-eval #:label #f 
(assume (positive? (sum xs)))
(verify
 (assert (ormap positive? xs)))]

This query returns @racket[(unsat)], as expected, despite
the bug in @racket[sum]. To see why, recall that
@racket[(verify #, @var{expr})] searches for an input that
violates an assertion in @var{expr}, while satisfying all
the assumptions and assertions accumulated in the
verification condition @racket[(vc)] before the call to
@racket[verify]. So, our query is @racket[unsat?] because
@racket[(ormap positive? xs)] holds whenever
@racket[(sum xs)] successfully computes a positive value.

A basic test suite, adapted from the
@seclink["sec:errors-under-queries"]{previous section}, will
not uncover this bug. If we run the tests against the new
@racket[sum], all the checks pass:

@examples[#:eval rosette-eval #:label #f 
(eval:no-prompt
(define (pre xs)
  (assume (positive? (sum xs))))
  
(define (post xs)
  (assert (ormap positive? xs)))

(define (query xs)
  (pre xs)
  (verify (post xs)))

(define example-tests 
  (test-suite
   "An example suite for a sum query."
   #:before clear-vc!
   #:after  clear-vc!

   (test-case
    "Test sum with concrete values."
    (check = (sum '()) 0)
    (check = (sum '(-1)) -1)
    (check = (sum '(-2 2)) 0)
    (check = (sum '(-1 0 3)) 2))

   (test-case
    "Test query post for exceptions."
    (before
     (clear-vc!)
     (check-not-exn (thunk (pre xs)))))
      
   (test-case
    "Test query post for exceptions."
    (before
     (clear-vc!)
     (check-not-exn (thunk (post xs)))))
  
   (test-case
    "Test query outcome."
    (before
     (clear-vc!)
     (check-pred unsat? (query xs)))))))

(eval:alts
 (run-test example-tests)
 (format-opaque "~a" (run-test example-tests)))
] 

One way to detect bugs of this kind is to run a "unit
verification query" for each key procedure in the program,
searching for assertion failures where none are expected:

@examples[#:eval rosette-eval #:label #f 
(test-case
 "Test sum for any failures."
 (check-pred unsat? (verify (sum xs))))
]

Another strategy is to avoid issuing any assumptions or
assertions outside of queries:
@examples[#:eval rosette-eval #:label #f 
(verify
 (begin
   (assume (positive? (sum xs)))
   (assert (ormap positive? xs))))]

But neither strategy is always possible, or
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

To run the error tracer on a program file @nonterm{prog},
use the @exec{raco} command:

@commandline{raco symtrace @nonterm{prog}}

The error tracer will open a web browser and stream
all exceptions that Rosette intercepted. For instance,
here is the output from the error tracer when running our
last query on the buggy @racket[sum] example from the
@seclink["sec:errors-under-symbolic-eval"]{previous section}:

@examples[#:eval rosette-eval #:label #f #:no-prompt
(assume (positive? (sum xs)))
(verify
 (assert (ormap positive? xs)))]

@(image interface.png #:scale 0.5)

The output shows a table of exceptions that Rosette
intercepted; here, there is one only exception, which is caused by our bug,
so there is only one row.
Each row consists of a shorter error message and an error location
(source file, line, and column). All rows can be expanded to show
more details: the full error message, the stack trace,
and the erroring (blamed) expression.  

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

Similarly, by default, the error tracer instruments only code that
does not belong to installed packages. To instrument
given installed packages, use the @DFlag{pkg} flag described below.

The @exec{raco symtrace @nonterm{prog}} command accepts the following command-line flags:
@itemlist[
 @item{@DFlag{module} @nonterm{module-name} --- run the
  specified @nonterm{module-name} submodule of @nonterm{prog}
  (defaults to the @tt{main} submodule).}

 @item{@DFlag{racket} --- instrument code in any language, not
  just those derived from Rosette.}

 @;{@item{@DFlag{solver} --- do not show exceptions raised on
  infeasible paths, using the solver to decide if paths are
  feasible. This option can cause significant performance degradation.}}

 @item{@DFlag{assert} --- do not show exceptions due to
  assertion errors, which are usually expected exceptions.}

 @item{@DFlag{pkg} @nonterm{pkg-name} --- instrument code in @nonterm{pkg-name}.}
  ]


Inside the web browser, the output can be customized further.
@itemlist[
  @item{The @bold{Group similar rows} switch will @emph{heuristically}
        group similar rows together, enabling easier navigation
        when many exceptions originate from the same place and due to the same cause.}
  @item{The @bold{Show Racket stacktrace} switch will display the top 32 entries of
        the Racket stack trace in addition to the Rosette stack trace.
        The Racket stack trace includes the details of evaluating Rosette's internal procedures,
        which the Rosette trace omits. These details are usually not necessary for 
        understanding errors in Rosette code, so the switch is off by default.}
  @item{The search box can be used to find rows that include the search string in their error message.}
]

@section{Walkthrough: Tracing Errors in Rosette}

To illustrate a typical error tracing process, consider
verifying the following buggy implementation of the
@link["https://en.wikipedia.org/wiki/Quickselect"]{
 quickselect algorithm}.

@examples[#:eval rosette-eval #:label #f #:no-prompt
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
            [(< n len<) (select <pivot)] (code:comment "Bug: should be (select <pivot n).")
            [else (select >=pivot (- n len< 1))])]))

(define-symbolic n k integer?)

(assume
 (and (<= 0 n (sub1 (length xs)))
      (= k (select xs n))))

(verify
 (assert (= k (list-ref (sort xs <) n))))
]

As before, the verification query succeeds despite the bug.
But unlike before, the bug is harder to detect. So we
run the error tracer on it and obtain the following output:

@(image quickselect.png #:scale 0.5)

The output from the error tracer includes 8 exceptions. Four
are arity mismatch exceptions that are due to the bug, and
the rest are benign assertion failures that cannot happen in
our example.

Because benign assertion failures are so common, the error
tracer provides an option to heuristically suppress them
from the output via the
@seclink["sec:symtrace:opts"]{@DFlag{assert}} flag. With the
flag enabled, the output contains only the four arity
mismatch exceptions.

Some assertion failures are bugs, however, so filtering with
@DFlag{assert} can end up hiding true positives and should
be used with this caveat in mind.


