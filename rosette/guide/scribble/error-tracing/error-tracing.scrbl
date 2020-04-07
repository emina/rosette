#lang scribble/manual

@(require scribble/core scribble/html-properties
          scribble/bnf scribble/eval
          (for-label (except-in racket list-set) errortrace
                     rosette/base/core/term
                     rosette/base/form/define
                     rosette/query/form
                     (only-in rosette/query/debug debug)
                     rosette/base/core/union
                     (only-in rosette unsat model evaluate)
                     (only-in rosette/query/finitize current-bitwidth)
                     (only-in rosette/base/base bitvector)
                     (only-in rosette/base/core/safe assert)
                     (only-in rosette/base/core/forall for/all))
          racket/runtime-path
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "error-tracer-log") #f 'rosette))

@title[#:tag "ch:error-tracing"]{Error Tracing}

Exceptions regularly occurred during Rosette program evaluation. To maintain
the correctness of symbolic evaluation, Rosette needs to intercept exceptions
under some context. An unintended side-effect is that errors from real mistakes
could be silently suppressed. This could cause an inconvenience in debugging,
and at worst, could make bugs go undetected. This chapter details common pitfalls,
best practice for Rosette program development, and an error tracer
for symbolic evaluation which allows users to see errors that Rosette intercepted.

@section[#:tag "sec:errors-in-rosette"]{Common Issues with Errors in Rosette}

Rosette intercepted exceptions primarily in two places.

@subsection{Solver-Aided Queries}

@seclink["ch:syntactic-forms:rosette"]{Solver-aided queries} (@racket[solve], @racket[verify], @racket[optimize],
@racket[synthesize], and @racket[debug]) need to intercept immediate exceptions within the form.
The @racket[verify] query, for instance, will return a counterexample that leads to a failure.
Therefore, the form needs to be able to handle exceptions that occur within the form.

Unintentional mistakes that cause errors will often lead to @racket[(unsat)] or a trivial counterexample @racket[(model)].
Consider a (somewhat convoluted) Rosette program that verifies that the sum of a list of length 4 should be the same
as the sum of the same list with zeros removed:

@interaction[#:eval rosette-eval
(define-symbolic xs integer? [4])
(define-symbolic k integer?)
(define (sum xs) (foldl + 0 xs))
(verify
 #:assume (assert (= k (sum xs)))
 #:guarantee (assert (= k (sum (filter (compose not zero?) xs)))))]

The above program is correct. However, with a slight typo by omitting the base @racket[0] in @racket[foldl], it might be surprising that no error occurs. In fact, the output is still @racket[(unsat)], making it appear that it is still verified.

@interaction[#:eval rosette-eval
(define (sum-buggy xs) (foldl + xs))
(verify
 #:assume (assert (= k (sum-buggy xs)))
 #:guarantee (assert (= k (sum-buggy (filter (compose not zero?) xs)))))]

The reason behind this output is that an error occurs in the @racket[#:assume] part of the @racket[verify] query, which signals a false assumption, resulting in a trivial verification.

As another example, consider a variant of the above buggy query:

@interaction[#:eval rosette-eval
(verify (assert (= (sum-buggy xs)
                   (sum-buggy (filter (compose not zero?) xs)))))]

The output is now a trivial counterexample, because even without any binding to symbolic constants, a failure occurs.

Errors that are intercepted in this manner could be uncover relatively easily by evaluating expressions outside of the solver-aided query forms. For example, evaluating either @racket[(assert (= k (sum-buggy xs)))] or @racket[(assert (= (sum-buggy xs) (sum-buggy (filter (compose not zero?) xs))))] would expose the bug.

@subsection[#:tag "sec:errors-under-symbolic-eval"]{Symbolic Evaluation}

Symbolic evaluation needs to intercept exceptions in order to mark branches where the exceptions occur as infeasible. It is in general not easy to identify this kind of errors.

As an example, consider the program that verifies that a list up to length 4 will have the same length as itself after a @racket[map] operation.

@interaction[#:eval rosette-eval
(define (len xs)
  (cond
    [(empty? xs) 0]
    [else (add1 (len (rest xs)))]))

(define-symbolic n integer?)
(define ys (take xs n))
(verify
 #:assume (assert (= k (len ys)))
 #:guarantee (assert (= k (len (map add1 ys)))))
]

Again, the above program is correct. However, with a slight mistake by changing @racket[rest] to @racket[first], it might be
surprising that no error occurs. In fact, the output is still @racket[(unsat)], making it appear that it is still verified.

@interaction[#:eval rosette-eval
(define (len-buggy ys)
  (cond
    [(empty? ys) 0]
    [else (add1 (len-buggy (first ys)))]))

(verify
 #:assume (assert (= k (len-buggy ys)))
 #:guarantee (assert (= k (len-buggy (map add1 ys)))))
]

The reason behind this output is that calling @racket[first] on an integer (in the recursive call) results in an error.
This causes symbolic evaluation to add a constraint that @racket[ys] must be an empty list in the
@racket[#:assume] part of the @racket[verify] query. With this (incorrect) assumption, the @racket[#:guarantee] part holds.

As mentioned above, it is relatively difficult to detect this kind of mistakes. Evaluating @racket[(assert (= k (len-buggy ys)))]
doesn't yield any apparent error. One could inspect the assertion store to find that the assertions in the store are incorrect, but this is usually unattainable due to how large the assertion store usually is.

@section[#:tag "sec:best-practice"]{Best Practice}

It is recommended that Rosette users perform a basic sanity check that the query sent to the solver is correct. This includes the following:

@subsection{Tests with Known Results}

Tests could be an effective way to expose mistakes. They should consist of queries that are @emph{known} to result in @racket[(unsat)] and queries that are known to @emph{known} to result in a @racket[model].

For instance, in the first example of the previous section, we know that @racket[(sum xs)] should not always be zero. Therefore, we expect the following query to return a counterexample:

@interaction[#:eval rosette-eval
(verify
 #:assume (assert (= k (sum xs)))
 #:guarantee (assert (= k 0)))
]

However, when we apply the same process with the buggy variant, the result remains @racket[(unsat)]. This indicates that there is a problem with the query.

@interaction[#:eval rosette-eval
(verify
 #:assume (assert (= k (sum-buggy xs)))
 #:guarantee (assert (= k 0)))
]

@subsection{Model Enumeration}

One could enumerate models to check the integrity of a query. For instance, in the last example of the previous section,
we can try enumerating models satisfying @racket[(assert (= k (len ys)))]

@interaction[#:eval rosette-eval
(define mod (solve (assert (= k (len ys)))))
mod
(solve (assert (and (= k (len ys)) (not (= k (evaluate k mod))))))
]

However, when we apply the same process with the buggy variant, we discover that some expected models are missing, showing
that there is a mistake.

@interaction[#:eval rosette-eval
(define mod (solve (assert (= k (len-buggy ys)))))
mod
(solve (assert (and (= k (len-buggy ys)) (not (= k (evaluate k mod))))))
]

@section[#:tag "sec:error-tracer"]{Error Tracer}

Rosette provides an @deftech[#:key "error tracer"]{error tracer},
which allows users to see errors that Rosette intercepted, both under solver-aided queries and symbolic evaluation.
Rosette users are encouraged to run it and look at
its output to make sure that there are no unexpected errors.
Note that exceptions reported by the error tracer could be benign. That is to say,
there could be false positive. The presence of exceptions thus does not guarantee
that there is a mistake in a program.

@bold{Running the error tracer} has the same limitations as @racketmodname[errortrace] and the @seclink["sec:sympro"]{symbolic profiler}:
before running it, throw away any @filepath{.zo} versions of your program
(i.e., delete any @filepath{compiled} folders in your code's folder hierarchy).

Then invoke the symbolic profiler on a program file @nonterm{prog} using @exec{raco}:

@commandline{raco symtrace @nonterm{prog}}

After executing @nonterm{prog},
the error tracer will at the end output all exceptions that Rosette intercepted.
For instance, the output from the error tracer when running on the @seclink["sec:errors-under-symbolic-eval"]{@racket[len-buggy]} example is as follows:

@verbatim|{
--------------------------------------------------------------------------------
exn: first: contract violation
  expected: #<procedure:list?>
  given: xs@0
  context...:
   @first
   ~/rosette/rosette/lib/trace/tool.rkt:121:5
   /Applications/Racket v7.6/collects/racket/private/more-scheme.rkt:265:2: call-with-exception-handler
   ...

at: ~/rosette/test/trace/code/all/ex-2.rkt line 6 column 27
(first ys)

call stack:
~/rosette/test/trace/code/all/ex-2.rkt:6:27 @first
~/rosette/test/trace/code/all/ex-2.rkt:6:16 len-buggy
~/rosette/test/trace/code/all/ex-2.rkt:14:23 len-buggy

--------------------------------------------------------------------------------
exn: first: contract violation
  expected: #<procedure:list?>
  given: (+ 1 xs@0)
  context...:
   @first
   ~/rosette/rosette/lib/trace/tool.rkt:121:5
   /Applications/Racket v7.6/collects/racket/private/more-scheme.rkt:265:2: call-with-exception-handler
   ...

at: ~/rosette/test/trace/code/all/ex-2.rkt line 6 column 27
(first ys)

call stack:
~/rosette/test/trace/code/all/ex-2.rkt:6:27 @first
~/rosette/test/trace/code/all/ex-2.rkt:6:16 len-buggy
~/rosette/test/trace/code/all/ex-2.rkt:15:26 len-buggy
}|

The output shows a list of exceptions that Rosette intercepted (here, there are two exceptions). For each exception, there are three parts:

@itemlist[
  @item{The exception message}
  @item{The expression that causes the exception}
  @item{The call stack when the exception occurs}
]

@subsection[#:tag "sec:symtrace:opts"]{Options and Caveats}

Just like the symbolic profiler, by default, the error tracer instruments only code that is within
a module whose initial module path is either @tt{rosette} or @tt{rosette/safe}.
In other words, only files beginning with @tt{#lang rosette} or @tt{#lang rosette/safe} will be instrumented.
This means that the shown call stacks and expressions will not include non-instrumented files.
To instrument @emph{all} code, use the @DFlag{racket} flag described below.

The @exec{raco symtrace @nonterm{prog}} command accepts the following command-line flags:

@itemlist[
 @item{@DFlag{stream} --- stream the intercepted exception while executing
   the program, rather than producing output only once the program completes.
   This option is useful for programs that do not terminate, or take a very long
   time to run. Note however that the output can interleave with regular output from program execution, which might not be desirable.}

 @item{@DFlag{module} @nonterm{module-name} --- run the specified @nonterm{module-name}
   submodule of @nonterm{prog} (defaults to the @tt{main} submodule).}

 @item{@DFlag{racket} --- instrument code in any module, not just those
   derived from Rosette.}

 @item{@DFlag{infeasible} --- skip exceptions due to apparent infeasible paths.}

 @item{@DFlag{solver} --- skip exceptions due to infeasible paths via a solver (not taking the assertion store into account). Note that using this option could slow down the program execution and error tracer significantly.}

 @item{@DFlag{assert} --- skip exceptions due to assertion errors, which usually are expected exceptions.}

 @item{@DFlag{context} @nonterm{context-length} --- set the length of the exception context (defaults to 3)}
  ]

@section{Walkthrough: Tracing Errors in Rosette}

To illustrate a typical error tracing process,
consider the @seclink["sec:errors-under-symbolic-eval"]{@racket[len-buggy]} example above along with the @seclink["sec:error-tracer"]{output} from the error tracer. From the error message, both exceptions indicate that during symbolic evaluation, @racket[first] is applied to a non-@racket[list]. The exceptions also pinpoint exactly the buggy expression and its location: @racket[(first ys)]. Sometimes, the call stack could be very helpful to understand how the bug is triggered.

As another example, consider a more complicated program that verifies that the @link["https://en.wikipedia.org/wiki/Quickselect"]{quickselect algorithm} is correct.

@interaction[#:eval rosette-eval
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
            [(< n len<) (select <pivot n)]
            [else (select >=pivot (- n len< 1))])]))

(verify
 #:assume (assert (and (<= 0 n (sub1 (length xs)))
                       (= k (select xs n))))
 #:guarantee (assert (= k (list-ref (sort xs <) n))))
]

Again, the above program is correct. However, with a slight mistake by omitting an argument of the recursive call of @racket[select], it might be
surprising that no error occurs. In fact, the output is still @racket[(unsat)], making it appear that it is still verified.

@interaction[#:eval rosette-eval
(define (select-buggy xs n)
  (cond
    [(empty? xs) (assert #f "unexpected empty list")]
    [else (define pivot (first xs))
          (define non-pivot (rest xs))
          (define <pivot (filter (λ (x) (< x pivot)) non-pivot))
          (define >=pivot (filter (λ (x) (>= x pivot)) non-pivot))
          (define len< (length <pivot))
          (cond
            [(= n len<) pivot]
            [(< n len<) (select-buggy <pivot)]
            [else (select-buggy >=pivot (- n len< 1))])]))

(verify
 #:assume (assert (and (<= 0 n (sub1 (length xs)))
                       (= k (select-buggy xs n))))
 #:guarantee (assert (= k (list-ref (sort xs <) n))))
]

Just like the @racket[len-buggy] example, it is relatively difficult to detect the bug in this program.
However, running the error tracer on the program produces the following output:

@verbatim|{
--------------------------------------------------------------------------------
exn: select-buggy: arity mismatch;
 the expected number of arguments does not match the given number
  expected: 2
  given: 1
  arguments...:
   {[(&& (< xs@1 xs@0) (< xs@2 xs@0) (< ...)) (xs@1 xs@2 xs@3)] ...
  context...:
   ~/rosette/rosette/lib/trace/tool.rkt:121:5
   /Applications/Racket v7.6/collects/racket/private/more-scheme.rkt:265:2: call-with-exception-handler
   ~/rosette/rosette/lib/trace/compile.rkt:267:11
   ...

at: ~/rosette/test/trace/code/all/ex-3.rkt line 16 column 24
(select-buggy <pivot)

call stack:
~/rosette/test/trace/code/all/ex-3.rkt:16:24 select-buggy
~/rosette/test/trace/code/all/ex-3.rkt:21:28 select-buggy

--------------------------------------------------------------------------------
exn: assert: unexpected empty list
  context...:
   ~/rosette/rosette/lib/trace/compile.rkt:267:11
   /Applications/Racket v7.6/collects/racket/private/more-scheme.rkt:265:2: call-with-exception-handler
   .../more-scheme.rkt:261:28
   ...

at: ~/rosette/test/trace/code/all/ex-3.rkt line 8 column 18
(assert #f "unexpected empty list")

call stack:
~/rosette/test/trace/code/all/ex-3.rkt:17:18 select-buggy
~/rosette/test/trace/code/all/ex-3.rkt:21:28 select-buggy

--------------------------------------------------------------------------------

[6 exceptions similar to above exceptions skipped]

--------------------------------------------------------------------------------
exn: assert: both branches infeasible
  context...:
   .../more-scheme.rkt:261:28
   ~/rosette/rosette/base/form/control.rkt:33:0: branch-and-merge
   ~/rosette/rosette/lib/trace/compile.rkt:267:11
   ...

at: ~/rosette/test/trace/code/all/ex-3.rkt line 14 column 10
(cond
  [(= n len<) pivot]
  [(< n len<) (select-buggy <pivot)]
  [else (select-buggy >=pivot (- n len< 1))])

call stack:
~/rosette/test/trace/code/all/ex-3.rkt:17:18 select-buggy
~/rosette/test/trace/code/all/ex-3.rkt:17:18 select-buggy
~/rosette/test/trace/code/all/ex-3.rkt:17:18 select-buggy
~/rosette/test/trace/code/all/ex-3.rkt:21:28 select-buggy
}|

The output from the error tracer contains a considerable amount of noise. In particular, assertion failures are usually intentional errors and therefore false positives, so showing them might not always be desirable.
Luckily, the first exception is faulty and it shows the location where the mistake occurs, so it is still easy to see the mistake clearly. However, we might not be fortunate to find a faulty exception so quickly like this in general.

The error tracer has an option to skip exceptions due to assertion errors via the @seclink["sec:symtrace:opts"]{@DFlag{--assert}} flag. With the flag enabled, the output now contains only exceptions that are related to @racket[select-buggy]. This allows us to see potential errors more clearly. The option @DFlag{--infeasible} and @DFlag{--solver} also filter exceptions that are likely to be false positives due the @seclink["sec:state-reflection"]{infeasiblity of symbolic state}. Note that the @DFlag{--solver} flag is very expensive as it uses the solver to prune infeasible paths.