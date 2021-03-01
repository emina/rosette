#lang scribble/manual

@(require (for-label racket (only-in racket/sandbox with-deep-time-limit))
          (for-label  
           rosette/base/form/define
           (only-in rosette/base/base assert assume vc vc-asserts vc-assumes clear-vc!)
           rosette/query/query 
           (only-in rosette/base/base bv? bitvector
                    bvsdiv bvadd bvsle bvsub bvand
                    bvor bvxor bvshl bvlshr bvashr
                    bvnot bvneg)
           rosette/lib/synthax))

@(require racket/sandbox  racket/runtime-path  scribble/core scribble/racket
          scribble/example scribble/html-properties scriblib/footnote 
          (only-in racket [unsyntax racket/unsyntax]))

@(require (only-in "../refs.scrbl" ~cite rosette:onward13 rosette:pldi14)
          "../util/lifted.rkt")
 
@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "essentials-log")))

@(define (symbolic s) @racketresultfont[s]) 

@(define-footnote footnote footnote-part)

@title[#:tag "ch:essentials"]{Rosette Essentials}

Rosette adds to Racket a collection of solver-aided facilities.
These facilities enable programmers to conveniently access a constraint solver 
that can answer interesting questions about program behaviors.  They are based on four 
key concepts: @emph{symbolic values}, @emph{assertions}, @emph{assumptions}, and @emph{queries}. 
We use assertions and assumptions to express desired program behaviors and symbolic values to 
formulate queries about these behaviors.

This chapter illustrates the basics of solver-aided programming.
More advanced tutorials, featuring extended examples, can be found 
in Section 2 of @~cite[rosette:onward13 rosette:pldi14].@footnote{Code examples in  
these references are written in earlier versions of Rosette.
While Rosette 4 is not backward compatible with these versions,
they share the same conceptual core.} 

The following chapters describe the subset 
of Racket that can be @seclink["sec:langs"]{safely} used with solver-aided facilities, including the 
supported datatypes (both @seclink["ch:built-in-datatypes"]{built-in}
and @seclink["ch:programmer-defined-datatypes"]{programmer-defined}), 
@seclink["ch:syntactic-forms"]{syntactic forms}, and @seclink["ch:libraries"]{libraries}. 

@section[#:tag "sec:symbolic-values"]{Symbolic Values}

The Rosette language includes two kinds of values: concrete and symbolic.  Concrete values are plain Racket values (@racket[#t], @racket[#f], @racket[0], @racket[1], etc.), and Rosette programs that operate only on concrete values behave like Racket programs.  Accessing the solver-aided features of Rosette---such as code synthesis or verification---requires the use of symbolic values.

@deftech[#:key "symbolic constant"]{Symbolic constants} are the simplest kind of symbolic value.  They can be created using the @racket[define-symbolic] form:
@examples[#:eval rosette-eval #:label #f
(define-symbolic b boolean?)
b]
This generates a fresh symbolic constant of type boolean and binds it to the variable @racket[b].

You can think of a symbolic constant as a placeholder for a concrete constant of the same type. As we will see shortly, the solver, once called, determines which concrete value a given symbolic constant represents:  it will tell us whether the constant @symbolic{b} is @racket[#t] or @racket[#f], depending on what question we ask about the behavior of a program (or a procedure) applied to @symbolic{b}.

Symbolic values, including constants, can be used just like concrete values of the same type.  They can be stored in data structures or passed to procedures to obtain other values, either concrete or symbolic:
@examples[#:eval rosette-eval #:label #f
(boolean? b)     
(integer? b)
(vector b 1)
(not b) 
(boolean? (not b))]
In our example, all but the fourth expression produce concrete values.  The fourth expression returns another symbolic value---specifically, a symbolic @emph{expression} of type boolean.  This expression represents the negation of @symbolic{b}.  If the solver determines that @symbolic{b} is @racket[#t], for example, then @symbolic{(! b)} will be interpreted as @racket[#f].


Rosette provides one more construct for creating symbolic constants besides @racket[define-symbolic]:
@examples[#:eval rosette-eval #:label #f #:no-prompt
(define-symbolic* n integer?)]
The two constructs differ in how they bind variables to constants when evaluated more than once.
The @racket[define-symbolic] form binds the variable to the same (unique) constant every time it is evaluated. The @racket[define-symbolic*] form, in contrast, creates a stream of (unique) constants, binding the variable to the next constant from its stream whenever the form is evaluated. The following example illustrates the difference:
@examples[#:eval rosette-eval #:label #f
(eval:no-prompt
 (define (static) 
  (define-symbolic x boolean?)  (code:comment "Creates the same constant when evaluated.")
  x))

(eval:no-prompt
 (define (dynamic) 
  (define-symbolic* y integer?) (code:comment "Creates a fresh constant when evaluated.")
  y))

(eq? (static) (static))
(eq? (dynamic) (dynamic))]

Printed constant names, such as @symbolic{x} or @symbolic{b}, are just comments.  Two constants created by evaluating two distinct @racket[define-symbolic] (or, @racket[define-symbolic*]) forms are distinct, even if they have the same printed name.  They may still represent the same concrete value, but that is determined by the solver:

@examples[#:eval rosette-eval #:label #f
(eval:no-prompt
 (define (yet-another-x) 
  (define-symbolic x boolean?) 
  x))

(eq? (static) (yet-another-x))]



@section[#:tag "sec:asserts"]{Assertions and Assumptions}

Like many languages, Rosette provides a construct for expressing @emph{assertions}---important properties of programs that are checked in every execution.  Rosette assertions work just like Java or Racket assertions when given a concrete value:  if the value is false, the execution terminates with a runtime exception.  Otherwise, the execution proceeds normally.  
@examples[#:eval rosette-eval #:label #f
(assert #t) 
(eval:error (assert #f))]

When given a symbolic boolean value, however, a Rosette assertion has no immediate effect.  Instead, the value is accumulated in the current @tech{verification condition} (VC), and the assertion's effect (whether it passes or fails) is eventually determined by the solver.

@examples[#:eval rosette-eval #:label #f
(code:line (vc-asserts (vc)) (code:comment "We asserted #f above, so the current VC reflects that."))
(code:line (clear-vc!)       (code:comment "Clear the current VC."))
(vc-asserts (vc))
(code:line (assert (not b))  (code:comment "Add the assertion (not b) to the VC."))
(vc-asserts (vc))
(clear-vc!)]

Assertions express properties that a program must satisfy on all @emph{legal} inputs. In Rosette, as in other solver-aided frameworks, we use @emph{assumptions} to describe which inputs are legal. If a program violates an assertion on a legal input, we blame the program. But if it violates an assertion on an illegal input, we blame the caller. In other words, a program is considered incorrect only when it violates an assertion on a legal input.

Assumptions behave analogously to assertions on both concrete and symbolic values. In the concrete case, assuming @racket[#f] aborts the execution with a runtime exception, and assuming a true value is equivalent to calling @racket[(void)]. In the symbolic case, the assumed value is accumulated in the current VC.

@examples[#:eval rosette-eval #:label #f
(assume #t)
(vc-assumes (vc))
(eval:alts
 (code:line (assume #f)         (code:comment "Assuming #f aborts the execution with an exception."))
 (eval:error (assume #f)))
(vc-assumes (vc))
(clear-vc!)
(define-symbolic i j integer?)
(code:line (assume (> j 0))     (code:comment "Add the assumption (> j 0) to the VC."))
(vc-assumes (vc))
(assert (< (- i j) i))
(code:line (vc-asserts (vc))    (code:comment "The assertions must hold when the assumptions hold."))
(code:line (vc)                 (code:comment "VC tracks the assumptions and the assertions."))]

@(rosette-eval '(clear-vc!))

@section[#:tag "sec:queries"]{Solver-Aided Queries}

The solver reasons about assumed and asserted properties only when we ask a question about them---for example, "Does my program have an execution that violates an assertion while satisfying all the assumptions?"  We pose such @emph{solver-aided queries} with the help of constructs explained in the remainder of this chapter.

We will illustrate the queries on the following toy example. Suppose that we want to implement a
procedure @racket[bvmid] that takes as input two non-negative 32-bit integers, @racket[lo] ≤ @racket[hi],
and returns the midpoint of the interval [@racket[lo], @racket[hi]]. In C or Java, we would declare
the inputs and output of @racket[bvmid] to be of type ``int''. In Rosette, we model finite precision
(i.e., machine) integers as @seclink["sec:bitvectors"]{bitvectors} of length 32.

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:comment "int32? is a shorthand for the type (bitvector 32).")
(define int32? (bitvector 32))]
@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:comment "int32 takes as input an integer literal and returns")
(code:comment "the corresponding 32-bit bitvector value.")
(define (int32 i)
  (bv i int32?))]
@examples[#:eval rosette-eval #:label #f 
(code:line (int32? 1)         (code:comment "1 is not a 32-bit integer"))
(code:line (int32? (int32 1)) (code:comment "but (int32 1) is."))
(int32 1)]

Bitvectors support the usual operations on machine integers, and we can use them to implement @racket[bvmid] as follows: 

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:comment "Returns the midpoint of the interval [lo, hi].") 
(define (bvmid lo hi)  (code:comment "(lo + hi) / 2")            
  (bvsdiv (bvadd lo hi) (int32 2)))]

As the above implementation suggests, we intend the midpoint to be the mathematical
integer @tt{mi = (lo + hi) / 2}, where @tt{/} stands for integer division. Assuming
that @tt{0 ≤ lo ≤ hi}, the midpoint @tt{mi} is fully characterized by two properties:
(1) @tt{lo ≤ mi ≤ hi} and (2) @tt{0 ≤ (hi - mi) - (mi - lo) ≤ 1}. We can use these
properties to define a generic correctness specification for any implementation of
@racket[bvmid] as follows: 

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:line
(define (check-mid impl lo hi)     (code:comment "Assuming that")
  (assume (bvsle (int32 0) lo))    (code:comment "0 ≤ lo and")             
  (assume (bvsle lo hi))           (code:comment "lo ≤ hi,") 
  (define mi (impl lo hi))         (code:comment "and letting mi = impl(lo, hi) and") 
  (define diff                     (code:comment "diff = (hi - mi) - (mi - lo),")
    (bvsub (bvsub hi mi)             
           (bvsub mi lo)))         (code:comment "we require that")
  (assert (bvsle lo mi))           (code:comment "lo ≤ mi,")
  (assert (bvsle mi hi))           (code:comment "mi ≤ hi,")
  (assert (bvsle (int32 0) diff))  (code:comment "0 ≤ diff, and")
  (assert (bvsle diff (int32 1)))) (code:comment "diff ≤ 1."))]

This is not the only way to specify the behavior of @racket[bvmid], and we will see an
alternative specification later on. In general, there are many ways to describe what it
means for a program to be correct, and often, these descriptions are partial: 
they constrain some aspects of the implementation (e.g., the output is positive)
without fully defining its behavior. In our example, @racket[check-mid] is a
@emph{full functional correctness specification} in that it admits exactly one output value for @racket[(impl lo hi)], namely, @tt{(lo + hi) / 2}.

Testing @racket[bvmid] against its specification on a few concrete legal inputs, we find
that it triggers no assertion failures, as expected:

@examples[#:eval rosette-eval #:label #f  
 (check-mid bvmid (int32 0) (int32 0))
 (check-mid bvmid (int32 0) (int32 1))
 (check-mid bvmid (int32 0) (int32 2))
 (check-mid bvmid (int32 10) (int32 10000)) ]

But does it work correctly on @emph{all} legal inputs?  The answer, as we will see below, is ``no''.
In fact, @racket[bvmid] reproduces  @hyperlink["https://en.wikipedia.org/wiki/Binary_search_algorithm#Implementation_issues"]{a famous bug}
that lurked for years in widely used C and Java implementations of binary search.
       

@subsection[#:tag "sec:verify"]{Verification}

How can we check if @racket[bvmid] satisfies its specification on all legal inputs? One approach is to enumerate all pairs of 32-bit integers with @racket[0 ≤ lo ≤ hi] and apply @racket[(check-mid bvmid hi lo)] to each. This approach is sound (it is guaranteed to find a bug if one exists), but a quick calculation shows that it is impractical even for our toy example: @racket[bvmid] has roughly 2.3 × 10@superscript{18} legal inputs. A better approach is to delegate such checks to a constraint solver, which can search  large input spaces much more effectively than naive enumeration.  In Rosette, this is done with the help of the @racket[verify] query:

@examples[#:eval rosette-eval #:label #f  
(eval:no-prompt (define-symbolic l h int32?))
(define cex (verify (check-mid bvmid l h)))
cex] 

The @racket[(verify #, @var[expr])] form queries the solver for a @deftech{binding} from symbolic constants to concrete values that causes the evaluation of @var[expr] to violate an assertion, while satisfying all the assumptions, when the bound symbolic constants are replaced with the corresponding concrete values. If such a binding exists, as it does in our case, it is called a @emph{counterexample}. 

Bindings are first-class values in Rosette, and they can be freely manipulated by programs.  We can also interpret any Rosette value with respect to a binding using the built-in @racket[evaluate] procedure:
@examples[#:eval rosette-eval #:label #f 
(define cl (evaluate l cex))
(define ch (evaluate h cex))
(list cl ch)
(code:comment "We can convert these values to integer? constants for debugging:")
(define il (bitvector->integer cl))
(define ih (bitvector->integer ch))
(list il ih)
(code:comment "Here is the computed midpoint:")
(define m (bvmid cl ch))
m
(bitvector->integer m) 
(code:comment "This is clearly wrong. We expect (il + ih) / 2 instead:")
(quotient (+ il ih) 2)
(code:comment "Expressed as a 32-bit integer, the correct answer is:")
(int32 (quotient (+ il ih) 2))
(code:comment "So, check-mid fails on (bvmid cl ch):")
(eval:error (check-mid bvmid cl ch))]
@(rosette-eval '(clear-vc!))

In our example, evaluating @racket[l] and @racket[h] with respect to @racket[cex] reveals that @racket[bvmid] fails to return the correct midpoint value, thus causing the first assertion in the @racket[check-mid] procedure to fail. The bug is due to overflow:  
the expression @racket[(bvadd lo hi)] in @racket[bvmid] produces a negative value in
the 32-bit representation when the sum of
@racket[lo] and @racket[hi] exceeds 2@\superscript{31}-1.

@examples[#:eval rosette-eval #:label #f 
(bvadd cl ch)
(bitvector->integer (bvadd cl ch))
(+ il ih)
(- (expt 2 31) 1)]
             
@(rosette-eval '(clear-vc!))

A common  @hyperlink["https://en.wikipedia.org/wiki/Binary_search_algorithm#Implementation_issues"]{solution}
to this problem is to calculate the midpoint as @tt{lo + ((hi - lo) / 2)}. It is easy to see that all intermediate values in this calculation are at most @racket[hi] when @racket[lo] and @racket[hi] are both non-negative, so no overflow can happen. We can also verify this with Rosette:

@examples[#:eval rosette-eval #:label #f 
(eval:no-prompt
 (define (bvmid-no-overflow lo hi)
  (bvadd lo (bvsdiv (bvsub hi lo) (int32 2)))))

(verify (check-mid bvmid-no-overflow l h))]

@subsection[#:tag "sec:synthesize"]{Synthesis}

The solution given in @racket[bvmid-no-overflow] avoids the overflow problem in @racket[bvmid] at the cost of performing an additional arithmetic operation. Both implementations also rely on signed division, which is slow and expensive compared to addition, subtraction, and bitwise operations. So our ideal implementation would be correct, small, and composed of only cheap arithmetic and bitwise operations. Does such an implementation exist?  To find out, we turn to Rosette's @racket[synthesize] query.


The synthesis query uses the solver to search for a correct program in a space of candidate implementations defined by a syntactic @deftech{sketch}. A sketch is a program with @deftech[#:key "hole"]{holes}, which the solver fills with expressions drawn from a specified set of options. For example, @racket[(?? int32?)] stands for a hole that can be filled with any 32-bit integer constant, so the sketch @racket[(bvadd x (?? int32?))] represents all 2@superscript{32} programs that add a 32-bit constant to the variable @racket[x]. Rosette also lets you define richer holes that can be filled with expressions from a given grammar. For example, here is a grammar of all @racket[int32?] expressions that consist of cheap arithmetic and bitwise operations:

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:line
 (require rosette/lib/synthax)     (code:comment "Require the sketching library."))
(code:line
 (define-grammar (fast-int32 x y)  (code:comment "Grammar of int32 expressions over two inputs:")
   [expr
    (choose x y (?? int32?)        (code:comment "<expr> := x | y | <32-bit integer constant> |")
            ((bop) (expr) (expr))  (code:comment "          (<bop> <expr> <expr>) |")
            ((uop) (expr)))]       (code:comment "          (<uop> <expr>)")
   [bop
    (choose bvadd bvsub bvand      (code:comment "<bop>  := bvadd  | bvsub | bvand |")
            bvor bvxor bvshl       (code:comment "          bvor   | bvxor | bvshl |")
            bvlshr bvashr)]        (code:comment "          bvlshr | bvashr")
   [uop
    (choose bvneg bvnot)])         (code:comment "<uop>  := bvneg | bvnot"))]

Using this grammar, we can sketch a fast implementation of the midpoint calculation as follows:

@examples[#:eval rosette-eval #:label #f #:no-prompt 
(eval:alts
  (define (bvmid-fast lo hi)
    (fast-int32 lo hi #:depth 2))
  (require (only-in rosette/guide/scribble/essentials/bvmid bvmid-fast)))]

The above sketch describes the space of all expressions from the @racket[fast-int32] grammar that have parse trees of depth at most 2. The depth argument is optional. If ommitted, Rosette will use the value of the @racket[(current-grammar-depth)] parameter to bound the depth of the expressions drawn from the grammar.

At this point, it is worth noting that holes and sketches are not fundamental concepts in Rosette. Instead, they are macros defined in terms of the core constructs we have already seen, symbolic constants and assertions. For example,  @racket[(?? int32?)] is syntactic sugar for @racket[(let () (define-symbolic #, @var[id] int32?) #, @var[id])], where @var[id] is an internally generated name. Similarly, @racket[(choose bvneg bvnot)] expands to @racket[(if (?? boolean?) bvneg bvnot)]. Finally, a grammar hole such as @racket[(fast-int32 lo hi #:depth 2)] inlines its productions @racket[#:depth] times to create a nested expression of the form @racket[(choose lo hi (?? int32?) ((choose bvadd ...) (choose ...) (choose ...)) ((choose bvneg bvnot) (choose ...)))]. Assigning concrete values to the symbolic constants generated by this expression has the effect of selecting a parse tree (of depth 2 in our example) from the hole's grammar. So, completing a sketch is a matter of finding a suitable binding for the symbolic constants generated by the holes.

With this in mind, we can query the solver for a completion of the @racket[bvmid-fast] sketch (if any) that satisfies our correctness specification:
@(rosette-eval '(require (only-in rosette/guide/scribble/util/demo print-forms-alt)))
@examples[#:eval rosette-eval #:label #f
(code:comment "Save the above definitions to a file before calling print-forms.")
(define sol
  (synthesize
   #:forall    (list l h)
   #:guarantee (check-mid bvmid-fast l h)))
sol
(eval:alts 
(print-forms sol)
(print-forms-alt sol))]

The synthesis query takes the form @racket[(synthesize #:forall #, @var[input] #:guarantee #, @var[expr])], where @var[input] lists the symbolic constants that represent inputs to a sketched program, and @var[expr] gives the correctness specification for the sketch. The solver searches for a binding from the hole (i.e., non-@var[input]) constants to values such that @var[expr] satisfies its assertions on all legal @var[input]s. Passing this binding to @racket[print-forms] converts it to a syntactic representation of the completed sketch.@footnote{@racket[print-forms] works only on sketches that have been saved to disk.} In our example, the synthesized program implements the midpoint calculation using the logical shift operation, i.e., the midpoint between @racket[lo] and @racket[hi] is calculated as @tt{(lo + hi) >>@subscript{u} 1}.

@(rosette-eval '(clear-vc!))

@subsection[#:tag "sec:solve"]{Angelic Execution}

Rosette supports one more solver-aided query, which we call angelic execution. This query is the dual of verification.  Given a program with symbolic values, it instructs the solver to find a binding for them that will cause the program to execute normally---that is, without any assumption or assertion failures. 

Angelic execution can be used to solve puzzles, to run incomplete code, or to "invert" a program, by searching for inputs that produce a desired output.  For example, we can ask the solver to search for two distinct legal inputs, @racket[l] and @racket[h], whose midpoint is the bitwise-and of their bits: 
@examples[#:eval rosette-eval #:label #f 
(define (bvmid-fast lo hi)
  (bvlshr (bvadd hi lo) (bv #x00000001 32)))

(define sol 
  (solve
   (begin
     (assume (not (equal? l h)))
     (assume (bvsle (int32 0) l))
     (assume (bvsle l h))
     (assert (equal? (bvand l h) (bvmid-fast l h))))))

sol

(evaluate (bvand l h) sol)
(evaluate (bvmid-fast l h) sol)]
As a fun exercise that builds on this result, try using program synthesis to discover the condition, @racket[(bvmid-and? l h)], that is both necessary and sufficient to ensure that @racket[bvand] and @racket[bvmid-fast] produce the same value on all distinct legal inputs @racket[l] and @racket[r]. (Hint: you can reuse the @racket[fast-int32] grammar from the previous section.)

@examples[#:eval rosette-eval #:label #f 
(eval:no-prompt
 (define (bvmid-and? lo hi)
  #f (code:comment "<--- replace with your sketch\n")
  ))

(eval:alts
(print-forms 
 (synthesize
  #:forall (list l h)
  #:guarantee
  (begin
    (assume (not (equal? l h)))
    (assume (bvsle (int32 0) l))
    (assume (bvsle l h))
    (assert
     (<=> (bvmid-and? l h)
          (equal? (bvand l h) (bvmid-fast l h)))))))
  (void))]

@(rosette-eval '(clear-vc!))

@section[#:tag "sec:notes"]{Symbolic Reasoning}

We conclude this chapter with a quick overview of common patterns and anti-patterns for programming in Rosette. For more details, see Chapters @seclink["ch:unsafe"]{8}--@seclink["ch:error-tracing"]{10}.

@subsection{Mixing Theories}

Rosette implements solver-aided queries by translating them to the input language of an SMT solver. By default, this translation respects types: a symbolic constant of type @racket[integer?] will be translated to an SMT constant of the same type, i.e., an infinite precision mathematical integer. These types determine which @emph{theories} the solver will need to use to solve a query. As a rule of thumb, the theory of bitvectors tends to elicit fastest solving times, and mixing theories can lead to severe performance degradation. For that reason, it is best to use the types from the same theory throughout your program (e.g., bitvectors).

To illustrate the impact of mixing theories, consider the following mixed-theory specification for our midpoint example:

@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:line
(define (check-mid-slow impl lo hi)      (code:comment "Assuming that")        
  (assume (bvsle (int32 0) lo))          (code:comment "0 ≤ lo and")                  
  (assume (bvsle lo hi))                 (code:comment "lo ≤ hi,")
  (assert                                (code:comment "we require that")
   (equal?                               
    (bitvector->integer (impl lo hi))    (code:comment "⌈impl(lo, hi)⌉ = ")
    (quotient                            (code:comment "(⌈lo⌉ + ⌈hi⌉) / 2, where")
     (+ (bitvector->integer lo)          (code:comment "⌈e⌉ stands for the mathematical")
        (bitvector->integer hi))         (code:comment "integer corresponding to the")
     2))))                               (code:comment "32-bit integer e."))]

This new specification uses both bitvectors and integers. Compared to @racket[check-mid], which uses only bitvectors, @racket[check-mid-slow] causes one of our verification queries to become an order of magnitude slower and the other to time out:

@(rosette-eval '(require (only-in racket error)))

@examples[#:eval rosette-eval #:label #f 
(time (verify (check-mid bvmid l h)))
(time (verify (check-mid-slow bvmid l h)))
(time (verify (check-mid bvmid-no-overflow l h)))
(eval:alts
(with-deep-time-limit 600 (code:comment "Timeout after 10 minutes ...")
  (verify (check-mid-slow bvmid-no-overflow l h)))
(eval:error (error 'call-with-deep-time-limit "out of time")))]


@subsection{Reasoning Precision}

While slower than bitvectors, integers are more convenient to use for demos, prototyping, and interfacing with Racket. To bridge this gap, Rosette provides the option of approximating symbolic integers (and reals) as bitvectors of length @var{k}, by setting the @racket[current-bitwidth] parameter to @var{k}. With this setting, integers (and reals) are treated as infinite precision values during evaluation, but when solving queries, they are translated to bitvectors of length @var{k} for better performance. 

For example, our slow midpoint queries become orders-of-magnitude faster when allowed to approximate integers with bitvectors:
@examples[#:eval rosette-eval #:label #f 
(code:comment "By default, current-bitwidth is set to #f, so Rosette translates")
(code:comment "integer? values precisely, using the SMT theory of integers.")
(current-bitwidth)
(code:comment "After we set current-bitwidth to 64, integer? values in")
(code:comment "check-mid-slow are translated to SMT bitvectors of length 64.")
(current-bitwidth 64)
(time (verify (check-mid-slow bvmid l h)))
(time (verify (check-mid-slow bvmid-no-overflow l h)))]

In this example, we have chosen @racket[current-bitwidth] carefully to ensure that the resulting approximation is both performant and sound---i.e., the approximate query returns a counterexample exactly when one would be returned by the corresponding integer query. But choosing the right bitwidth is difficult to do in general. If we underapproximate the number of bits that are needed to represent every integer value in a query, we lose soundness, and if we overapproximate it, we lose performance.

For instance, when we re-run the slow midpoint queries with @racket[current-bitwidth] set to 32, the buggy query fails to produce a counterexample and the correct query returns a bogus counterexample.  Both results are correct for 32-bit machine integers but incorrect for (infinite-precision) mathematical integers:
@examples[#:eval rosette-eval #:label #f 
(code:comment "The bitwidth is too low, so we get ...")
(current-bitwidth 32) 
(code:comment "no counterexample to a buggy query, and")
(time (verify (check-mid-slow bvmid l h)))
(code:comment "a bogus counterexample to a correct query.")
(time (verify (check-mid-slow bvmid-no-overflow l h)))]
We can restore soundness by sacrificing performance and setting @racket[current-bitwidth] conservatively to a large value (e.g., 512). In our case, the queries are small so an order-of-magnitude slowdown is acceptable. For large queries, this would lead to timeouts:
@examples[#:eval rosette-eval #:label #f 
(code:comment "The bitwidth is too high, so we get a 3-10X slowdown.")
(current-bitwidth 512) 
(time (verify (check-mid-slow bvmid l h)))
(time (verify (check-mid-slow bvmid-no-overflow l h)))]

In practice, it is usually best to leave @racket[current-bitwidth] at its default setting (@racket[#f]), and limit the use of integers to code that will be evaluated concretely. This approach works especially well when the solver is configured to accept only bitvectors, so if any integers have made it into a query, the solver fails fast:
@examples[#:eval rosette-eval #:label #f 
(current-bitwidth #f)
(require rosette/solver/smt/z3)
(code:line (current-solver (z3 #:logic 'QF_BV))       (code:comment "Allow only bitvectors."))
(code:line (time (verify (check-mid bvmid l h)))      (code:comment "Accepted."))
(eval:alts
(code:line (time (verify (check-mid-slow bvmid l h))) (code:comment "Rejected."))
(eval:error (time (verify (check-mid-slow bvmid l h)))))]

@(rosette-eval '(clear-vc!))
@(rosette-eval '(current-solver (z3)))

@subsection{Symbolic Evaluation}

The process by which Rosette turns a query into an SMT constraint is called @deftech{symbolic evaluation}. At a high level, Rosette's symbolic evaluation works by executing @emph{all} paths through a program, and collecting all the assumptions and assertions on these paths into the current verification condition @racket[(vc)]. The resulting @racket[(vc)] is then translated to the SMT language and passed to the solver.  This evaluation model has two practical implications on writing performant and terminating Rosette code.

First, if a program is slow or runs forever under standard (concrete) evaluation, it will perform at least as poorly under all-path (symbolic) evaluation. Second, if a program terminates quickly on all concrete inputs, it can still perform poorly or fail to terminate on symbolic inputs. So, extra care must be taken to ensure good performance and termination in the presence of symbolic values. 

To illustrate, consider the procedure @racket[bvsqrt] for computing the @hyperlink["https://en.wikipedia.org/wiki/Integer_square_root#Using_bitwise_operations"]{integer square root} of non-negative 32-bit integers. This procedure terminates on all concrete values of @racket[n] but runs forever when given a symbolic input:
@examples[#:eval rosette-eval #:label #f
(eval:no-prompt
 (define (bvsqrt n)
  (cond
    [(bvult n (int32 2)) n]
    [else
     (define s0 (bvshl (bvsqrt (bvlshr n (int32 2))) (int32 1)))
     (define s1 (bvadd s0 (int32 1)))
     (if (bvugt (bvmul s1 s1) n) s0 s1)])))
(bvsqrt (int32 3))
(bvsqrt (int32 4))
(bvsqrt (int32 15))
(bvsqrt (int32 16))
(eval:alts
(with-deep-time-limit 10 (code:comment "Timeout after 10 seconds ...")
  (bvsqrt l))
(eval:error (error 'call-with-deep-time-limit "out of time")))]
The reason is simple: a call to @racket[bvsqrt] terminates when @racket[n] becomes less than 2. But if we start with a symbolic @racket[n], this never happens because Rosette right-shifts @racket[n] by 2 in each recursive call to generate a new symbolic value:
@examples[#:eval rosette-eval #:label #f
(define n0 l)
n0
(define n1 (bvlshr n0 (int32 2)))      
n1
(define n2 (bvlshr n1 (int32 2))) 
n2
(define n3 (bvlshr n2 (int32 2))) 
n3]
In general, recursion terminates under symbolic evaluation only when the stopping condition is reached with concrete values.

We can force termination by placing a concrete bound @var{k} on the number of times @racket[bvsqrt] can call itself recursively. This approach is called @deftech{finitization}, and it is the standard way to handle unbounded loops and recursion under symbolic evaluation. The following code shows how to implement a @emph{sound} finitization policy. If a @racket[verify] query returns @racket[(unsat)] under a sound policy, we know that (1) the unrolling bound @var{k} is sufficient to execute all possible inputs to  @racket[bvsqrt], and (2) all of these executions satisfy the query. If we pick a bound that is too small, the query will generate a counterexample input that needs a larger bound to compute the result. In our example, the bound of 16 is sufficient to verify the correctness of @racket[sqrt] on all inputs: 
@(rosette-eval '(clear-vc!))
@(rosette-eval '(require (only-in racket make-parameter parameterize)))
@examples[#:eval rosette-eval #:label #f #:no-prompt
(code:comment "Parameter that controls the number of unrollings (5 by default).")  
(define fuel (make-parameter 5)) 

(eval:no-prompt)
(code:comment "A simple macro for defining bounded procedures")
(code:comment "that use (fuel) to limit recursion.")
(define-syntax-rule
  (define-bounded (id param ...) body ...)
  (define (id param ...)
    (assert (> (fuel) 0) "Out of fuel.")
    (parameterize ([fuel (sub1 (fuel))])
      body ...)))

(eval:no-prompt)
(code:comment "Computes bvsqrt taking at most (fuel) steps.")
(define-bounded (bvsqrt n) 
  (cond
    [(bvult n (int32 2)) n]
    [else
     (define s0 (bvshl (bvsqrt (bvlshr n (int32 2))) (int32 1)))
     (define s1 (bvadd s0 (int32 1)))
     (if (bvugt (bvmul s1 s1) n) s0 s1)]))

(eval:no-prompt)
(code:comment "Correctness specification for bvsqrt:")
(code:line
(define (check-sqrt impl n)
  (assume (bvsle (int32 0) n))          (code:comment "Assuming n ≥ 0,")
  (define √n (impl l))                 
  (define √n+1 (bvadd √n (int32 1)))    (code:comment "we require that")
  (assert (bvule (bvmul √n √n) n))      (code:comment "(√n)^2 ≤ n and")
  (assert (bvult n (bvmul √n+1 √n+1)))) (code:comment "n < (√n + 1)^2."))]

@examples[#:eval rosette-eval #:label #f
(code:comment "Verification fails due to insufficient fuel.")
(define cex (time (verify (check-sqrt bvsqrt l))))
(eval:error (bvsqrt (evaluate l cex)))
(clear-vc!)
(code:comment "Verification succeeds with enough fuel.")
(fuel 16)
(time (verify (check-sqrt bvsqrt l)))]

Many other finitization policies can be defined in a similar way. For example, if we change @racket[define-bounded] to use @racket[assume] instead of @racket[assert], we obtain a finitization policy that ensures @emph{completeness}. If a @racket[verify] query returns a counterexample under a complete policy, we know that the program is buggy, and it violates a query assertion within @var{k} recursive calls. But if the query returns @racket[(unsat)], we know only that there are no bugs within @var[k] or fewer unrollings---we cannot conclude anything about longer executions. So a complete policy prevents false positives, while a sound one prevents false negatives. What kind of policy to use depends on the application, and Rosette leaves that choice to the programmer.


@(kill-evaluator rosette-eval)


@(footnote-part)
