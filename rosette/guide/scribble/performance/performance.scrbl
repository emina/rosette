#lang scribble/manual

@(require scribble/core scribble/html-properties
          scribble/bnf scribble/example
          (for-label (except-in racket list-set) errortrace
                     rosette/base/core/term
                     rosette/base/form/define
                     rosette/query/query
                     (only-in rosette/base/base bitvector assert for/all)
                     (only-in rosette current-bitwidth))
          racket/runtime-path
          "../util/lifted.rkt"
          (only-in "../refs.scrbl" ~cite sympro:oopsla18))

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "performance-log") #f 'rosette))

@(define-runtime-path profile.png "profile.png")
@(define-runtime-path profile-xform.png "profile-xform.png")

@(rosette-eval '(require (only-in rosette/guide/scribble/util/lifted format-opaque)))

@title[#:tag "ch:performance"]{Performance}


This chapter describes common performance problems in
Rosette programs, how to solve them, and how to diagnose
them with the built-in @tech{symbolic profiler}.

@section[#:tag "sec:antipatterns"]{Common Performance Issues}

When a Rosette program performs poorly,
it is often due to one of four common issues, described next.

@subsection{Integer and Real Theories}

Rosette supports assertions and assumptions containing
symbolic values of integer or real type. But solving queries
with these types is expensive (or, in the worst case,
undecidable), and even simple queries can be unacceptably
slow.

One solution is to change Rosette's
@racket[current-bitwidth] parameter, which controls the
@tech{reasoning precision} used for queries. When
@racket[current-bitwidth] is set to a value @emph{k} other
than @racket[#f], Rosette approximates @racket[integer?] and
@racket[real?] values using signed @emph{k}-bit
@racket[bitvector]s, which can make queries involving
integers and reals faster to solve.

But this approximation is unsound and may produce results
that are incorrect under the infinite-precision semantics of
integers and reals (while being correct under the
finite-precision semantics). For example, this program
incorrectly says that no integer greater than 15 exists,
because the setting of @racket[current-bitwidth] causes it
to consider only values of @racket{x} that can be
represented as a 5-bit bitvector.

@examples[#:eval rosette-eval #:label #f
(current-bitwidth 5)
(define-symbolic x integer?)
(solve (assert (> x 15)))]

So, choosing the right reasoning precision for an
application involves navigating this tradeoff between
performance and soundness.

@subsection{Algorithmic Mismatch}

Small algorithmic changes can have a large impact on the
efficiency of symbolic evaluation. Often, the most efficient
algorithm for symbolic inputs is different to the most
efficient one for concrete inputs---an @deftech{algorithmic
 mismatch}.

For example, consider this function to set the @tt{idx}th
element of a list @tt{lst} to @tt{val}:

@examples[#:eval rosette-eval #:label #f
(define (list-set lst idx val)
  (let-values ([(front back) (split-at lst idx)])
    (append front (cons val (cdr back)))))
(list-set '(a b c) 1 'd)
]

While appropriate for concrete inputs,
this function exhibits poor performance when the
inputs are symbolic:

@examples[#:eval rosette-eval #:label #f
(define-symbolic idx len integer?)
(define lst (take '(a b c) len))
(eval:alts
(code:line (list-set lst idx 'd) (code:comment "Symbolic union with 6 parts."))
(format-opaque "~a" (list-set lst idx 'd)))
]

The root cause is the @racket[split-at] operation, which
separates the front and back of the list into different
variables. Because the index @racket[idx] to split at is
symbolic, Rosette creates two @tech{symbolic unions} to
capture the possible front and back values as a function of
@racket[idx]. Even though the possible values of
@racket[front] and @racket[back] are related, this
separation loses the relationship.

A better implementation for symbolic inputs avoids splitting
the list by iterating over it, updating each position
depending on whether its index is equal to @tt{idx}:

@examples[#:eval rosette-eval #:label #f
(define (list-set* lst idx val)
  (for/all ([lst lst])
    (map (lambda (i v) (if (= idx i) val v))
         (build-list (length lst) identity)
         lst)))
(list-set* '(a b c) 1 'd)
(eval:alts
(code:line (list-set* lst idx 'd) (code:comment "Smaller symbolic union with 4 parts."))
(format-opaque "~a" (list-set* lst idx 'd)))
]

@subsection{Irregular Representation}

Just as the best algorithm for symbolic inputs can differ
from that for concrete inputs (an @tech{algorithmic
 mismatch}), so can the best data structure. Programming with
symbolic values is most efficient when data structures are
regular. Even though an @deftech{irregular representation}
may be more space efficient for concrete data, it can have
negative performance impacts when the data is symbolic.

For example, consider representing a (mutable) 2D grid data
structure using Rosette's lifted support for
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{vectors}.
The obvious representation is to use nested
vectors to represent the two dimensions:

@examples[#:eval rosette-eval #:label #f
(define-values (width height) (values 5 5))
(define-symbolic x y integer?)
(define grid/2d
  (for/vector ([_ height])
    (make-vector width #f)))
(vector-set! (vector-ref grid/2d y) x 'a)
]

This representation is inefficient when indexed with
symbolic values, because the dereferences are irregular: the
dereference of the @racket[y]-coordinate returns a vector,
whereas the dereference of the @racket[x]-coordinate returns
a value. This irregularity requires Rosette to perform more
symbolic evaluation work to faithfully track the usages of
the nested vector.

An alternative representation stores the entire grid in one vector,
indexed using simple arithmetic:

@examples[#:eval rosette-eval #:label #f
(define grid/flat
  (make-vector (* width height) #f))
(vector-set! grid/flat (+ (* y width) x) 'a)
]

This variant improves performance by about 2×.

@subsection{Missed Concretization}

In addition to employing careful algorithmic and
representational choices, fast solver-aided code provides as
much information as possible about the feasible choices of
symbolic values. Failure to make this information explicit
results in @deftech{missed concretization} opportunities,
leading to performance degradation.

For example, consider the following toy procedure,
that returns the @racket[idx]th element of the list @racket[lst],
but only if @racket[idx] is 0 or 1:

@examples[#:eval rosette-eval #:label #f
(define (maybe-ref lst idx)
  (if (<= 0 idx 1)
      (list-ref lst idx)
      -1))
(define-symbolic idx integer?)
(maybe-ref '(5 6 7) idx)
]

This procedure has poor performance when given a symbolic
index @racket[idx], because the call to
@racket[(list-ref lst idx)] passes a symbolic index, but the
conditional establishes that the only possible values for
that index are 0 or 1. When Rosette evaluates the
first side of the conditional, it does not simplify the
value of @racket[idx] to be only 0 or 1, and so the
resulting encoding creates infeasible branches for cases
where @racket[idx] is outside that range. An alternative
version captures that concreteness:

@examples[#:eval rosette-eval #:label #f
(define (maybe-ref* lst idx)
  (cond [(= idx 0) (list-ref lst 0)]
        [(= idx 1) (list-ref lst 1)]
        [else -1]))
(maybe-ref* '(5 6 7) idx)
]

This variant avoids generating infeasible return values
for the cases where @racket[idx] is greater than 1.

@section[#:tag "sec:sympro"]{Symbolic Profiling}

Rosette includes a @deftech[#:key "symbolic profiler"]{symbolic profiler}
for diagnosing performance issues in solver-aided programs.
The symbolic profiler instruments key metrics in Rosette's
symbolic evaluator and relates them to the performance of a
Rosette program, suggesting the locations of potential
bottlenecks---i.e., parts of the program that are
difficult to evaluate symbolically. More details about
symbolic profiling are available in the related technical
paper @~cite[sympro:oopsla18].

To run the symbolic profiler on a program file @nonterm{prog},
use the @exec{raco} command:

@commandline{raco symprofile @nonterm{prog}}

After executing @nonterm{prog},
the symbolic profiler produces a browser-based output summarizing the results,
similar to this output:

@(image profile.png #:scale 0.425)

The top half of this output visualizes the evolution of the Racket call stack
over time.
Each procedure call is a rectangle in this chart,
with its width corresponding to the total time taken by the call.
Blue highlighted regions reflect @tech{solver} activity
generated by Rosette @seclink["sec:queries"]{queries}.

The bottom half summarizes important metrics about the
symbolic evaluation on a per-procedure basis. Each procedure
invoked by the program has a row in the table, with columns
that measure:

@itemlist[
 @item{The total @bold{time} taken by all invocations of the procedure.}
 @item{The @bold{term count}, which is the total number of @tech[#:key "symbolic term"]{symbolic terms} created by the procedure.}
 @item{The number of @bold{unused terms}, which are terms created but never sent to a solver.}
 @item{The @bold{union size}, which is the total size of all @tech[#:key "symbolic unions"]{symbolic unions} created by the procedure.}
 @item{The @bold{merge cases}, which is the total number of execution paths merged by Rosette within the procedure.}
 ]

Procedures are ranked by a @bold{score}, which summarizes the other data in the table.
Procedures with higher scores are more likely to be bottlenecks,
and should be investigated first for performance issues.


@subsection[#:tag "sec:sympro:opts"]{Options and Caveats}

By default, the symbolic profiler instruments only code that is within
a module whose initial module path is either @tt{rosette} or @tt{rosette/safe}.
In other words, only files beginning with @tt{#lang rosette} or @tt{#lang rosette/safe} will be instrumented.
To instrument @emph{all} code, use the @DFlag{racket} flag described below.

Similarly, by default, the symbolic profiler instruments only code that
does not belong to installed packages. To instrument
given installed packages, use the @DFlag{pkg} flag described below.

The @exec{raco symprofile @nonterm{prog}} command accepts the following command-line flags:

@itemlist[
 @item{@DFlag{stream} --- stream profile data to a browser while executing
   the program, rather than producing output only once the program completes.
   This option is useful for programs that do not terminate, or take a very long
   time to run.}

 @item{@Flag{d} @nonterm{delay} --- delay between samples when using the @DFlag{stream} option,
   in seconds (defaults to 2 s).}

 @item{@Flag{m} @nonterm{module-name} --- run the specified @nonterm{module-name}
   submodule of @nonterm{prog} (defaults to the @tt{main} submodule).}

 @item{@Flag{t} @nonterm{threshold} --- prune function calls whose execution time is less
   than @nonterm{threshold} milliseconds (defaults to 1 ms).}

 @item{@DFlag{racket} --- instrument code in any language, not just those
   derived from Rosette.}

 @item{@DFlag{pkg} @nonterm{pkg-name} --- instrument code in @nonterm{pkg-name}.}
  ]




@section{Walkthrough: Debugging Rosette Performance}

To illustrate a typical Rosette performance debugging
process, consider building a small solver-aided program for
verifying optimizations in a toy calculator language. First,
we define the calculator language, in which programs are
lists of operations, and specify its semantics with a simple
recursive interpreter:

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:comment "Calculator opcodes.")
(define-values (Add Sub Sqr Nop)
  (values (bv 0 2) (bv 1 2) (bv 2 2) (bv 3 2)))

(code:comment "An interpreter for calculator programs.")
(code:comment "A program is a list of '(op) or '(op arg) instructions")
(code:comment "that update acc, where op is a 2-bit opcode and arg is")
(code:comment "a 4-bit constant.")
(define (calculate prog [acc (bv 0 4)])
  (cond                      ; An interpreter for 
    [(null? prog) acc]       ; calculator programs.
    [else                    ; A program is list of 
     (define ins (car prog)) ; '(op) or '(op arg)
     (define op (car ins))   ; instructions that up-
     (calculate              ; date acc, where op is 
      (cdr prog)             ; a 2-bit opcode and arg
      (cond                  ; is a 4-bit constant.
        [(eq? op Add) (bvadd acc (cadr ins))]
        [(eq? op Sub) (bvsub acc (cadr ins))]
        [(eq? op Sqr) (bvmul acc acc)]
        [else         acc]))]))]

One potential optimization for programs in this language is
to replace subtractions with additions. The @tt{sub->add}
procedure performs this operation at a given index in a
program:

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:comment "Functionally sets lst[idx] to val.")
(define (list-set lst idx val) ; Functionally sets
  (match lst                   ; lst[idx] to val.
    [(cons x xs)
     (if (= idx 0) 
         (cons val xs)
         (cons x (list-set xs (- idx 1) val)))]
    [_ lst]))

(code:comment "Replaces Sub with Add if possible.")
(define (sub->add prog idx)        ; Replaces Sub with 
  (define ins (list-ref prog idx)) ; Add if possible.
  (if (eq? (car ins) Sub)          
      (list-set prog idx (list Add (bvneg (cadr ins))))
      prog))]

To check that this optimization is correct, we implement a
tiny verification tool @tt{verify-xform} that constructs a
symbolic calculator program of size @tt{N}, applies the
optimization, and checks that the original and optimized
programs produce the same outputs:

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:comment "Verifies the given transform for all programs of length N.")
(define (verify-xform xform N) ; Verifies the given
  (define P                    ; transform for all 
    (for/list ([i N])          ; programs of length N.
      (define-symbolic* op (bitvector 2))   
      (define-symbolic* arg (bitvector 4))
      (if (eq? op Sqr) (list op) (list op arg))))
  (define-symbolic* acc (bitvector 4))
  (define-symbolic* idx integer?)
  (define xP (xform P idx))
  (verify ; ∀ acc, idx, P. P(acc) = xform(P, idx)(acc)
   (assert (eq? (calculate P acc) (calculate xP acc)))))
]

We can verify @tt{sub->add} for all calculator programs of size 5:

@examples[#:eval rosette-eval #:label #f
(verify-xform sub->add 5)
]

This produces no counterexamples, as expected.

@(rosette-eval '(clear-vc!))
@(rosette-eval '(clear-terms!))
@subsection{Performance Bottlenecks}

Verifying @tt{sub->add} for larger values of @tt{N} causes
the performance of @tt{verify-xform} to degrade, from a
couple of seconds when @tt{N} = 5 to over a minute when @tt{N}
= 20. To identify the source of this performance issue, we
can invoke the @tech{symbolic profiler} on the verifier,
producing the output below (after selecting the "Collapse
solver time" checkbox):

@(image profile-xform.png #:scale 0.425)

The symbolic profiler identifies @tt{list-set} as the
bottleneck in this program. The output shows that @tt{list-set}
creates many symbolic terms, and performs many symbolic
operations (the "Union Size" and "Merge Cases" columns).

The core issue here is an @tech{algorithmic mismatch}: @tt{list-set}
makes a recursive call guarded by a short-circuiting
condition @racket[(= idx 0)] that is symbolic when
@racket[idx] is unknown. When a condition's truth value is
unknown, Rosette must execute both branches of the
conditional, and then merge the resulting symbolic values.
So, in our example, Rosette always executes the recursive
call, and each call creates a larger symbolic value that
encodes the results of all the previous recursive calls.
This behavior leads to a quadratic growth in the symbolic
representation of the list returned by @tt{list-set}:

@examples[#:eval rosette-eval #:label #f 
(define-symbolic idx integer?)
(list-set '(1 2 3) idx 4)]

The solution is to alter @tt{list-set} to recurse unconditionally:

@examples[#:eval rosette-eval #:label #f #:no-prompt
(define (list-set* lst idx val)
  (match lst
    [(cons x xs)
     (cons (if (= idx 0) val x)
           (list-set* xs (- idx 1) val))]
    [_ lst]))]

In this revision, Rosette still evaluates both branches
of the conditional, but neither side of the conditional recurses,
and so the verification conditions no longer grow quadratically.

@examples[#:eval rosette-eval #:label #f
(list-set* '(1 2 3) idx 4)]

The performance of @tt{verify-xform} after this change
improves by 3× for @tt{N} = 20.