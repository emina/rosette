#lang scribble/manual

@(require (for-label racket)
          (for-label  
           rosette/base/form/define
           (only-in rosette/base/base assert assume vc spec-asserts spec-assumes clear-vc!)
           rosette/query/query  
           (only-in rosette/base/base bv? bitvector bvsdiv bvadd bvsle bvsub bvand bvor bvxor bvshl bvlshr bvashr bvnot bvneg)
           rosette/query/eval
           rosette/lib/synthax))

@(require racket/sandbox  racket/runtime-path  scribble/core scribble/racket
          scribble/eval scribble/html-properties ;scriblib/footnote 
          (only-in racket [unsyntax racket/unsyntax]))

@(require (only-in "../refs.scrbl" ~cite rosette:onward13 rosette:pldi14)
          "../util/lifted.rkt")
 
@(define-runtime-path root ".")
@(define rosette-eval (rosette-evaluator) #;(rosette-log-evaluator (logfile root "essentials-log")))

@(define (symbolic s) @racketresultfont[s]) 

@(define seen '())
@(define (footnote . xs)
   (define ord (add1 (length seen)))
   (define mark (superscript (format "~a" ord)))
   (define t (format "footnote~a" ord))
   (set! seen (cons (apply elemtag t mark xs) seen))
   (elemref t mark))
@(define (footnote-part)
   (let ([ts (reverse seen)])
     (if (null? ts)
         null
         (cons (para #:style (style #f (list (attributes '((class . "footnoteblock")))))
                     (car ts))
               (map para (cdr ts))))))
   
@;(define-footnote footnote footnote-part)

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
@def+int[#:eval rosette-eval
(define-symbolic b boolean?)
b]
This generates a fresh symbolic constant of type boolean and binds it to the variable @racket[b].

You can think of a symbolic constant as a placeholder for a concrete constant of the same type. As we will see shortly, the solver, once called, determines which concrete value a given symbolic constant represents:  it will tell us whether the constant @symbolic{b} is @racket[#t] or @racket[#f], depending on what question we ask about the behavior of a program (or a procedure) applied to @symbolic{b}.

Symbolic values, including constants, can be used just like concrete values of the same type.  They can be stored in data structures or passed to procedures to obtain other values, either concrete or symbolic:
@interaction[#:eval rosette-eval
(boolean? b)     
(integer? b)
(vector b 1)
(not b) 
(boolean? (not b))]
In our example, all but the fourth expression produce concrete values.  The fourth expression returns another symbolic value---specifically, a symbolic @emph{expression} of type boolean.  This expression represents the negation of @symbolic{b}.  If the solver determines that @symbolic{b} is @racket[#t], for example, then @symbolic{(! b)} will be interpreted as @racket[#f].


Rosette provides one more construct for creating symbolic constants besides @racket[define-symbolic]:
@def+int[#:eval rosette-eval
(define-symbolic* n integer?)]
The two constructs differ in how they bind variables to constants when evaluated more than once.
The @racket[define-symbolic] form binds the variable to the same (unique) constant every time it is evaluated. The @racket[define-symbolic*] form, in contrast, creates a stream of (unique) constants, binding the variable to the next constant from its stream whenever the form is evaluated. The following example illustrates the difference:
@defs+int[#:eval rosette-eval
((define (static) 
  (define-symbolic x boolean?)  (code:comment "Creates the same constant when evaluated.")
  x)

(define (dynamic) 
  (define-symbolic* y integer?) (code:comment "Creates a fresh constant when evaluated.")
  y))

(eq? (static) (static))
(eq? (dynamic) (dynamic))]

Printed constant names, such as @symbolic{x} or @symbolic{b}, are just comments.  Two constants created by evaluating two distinct @racket[define-symbolic] (or, @racket[define-symbolic*]) forms are distinct, even if they have the same printed name.  They may still represent the same concrete value, but that is determined by the solver:

@def+int[#:eval rosette-eval
(define (yet-another-x) 
  (define-symbolic x boolean?) 
  x)

; Produces a boolean expression whose meaning is 'true' if and only if the 
; constant returned by (static) and the constant returned by (yet-another-x)
; have the same concrete interpretation.
(eq? (static) (yet-another-x))]



@section[#:tag "sec:asserts"]{Assertions and Assumptions}

Like many other languages, Rosette provides a construct for expressing @emph{assertions}---important properties of programs that are checked in every execution.  Rosette assertions work just like Java or Racket assertions when given a concrete value:  if the value is false, the execution terminates with a runtime exception.  Otherwise, the execution proceeds normally.  
@interaction[#:eval rosette-eval
(assert #t) 
(assert #f)]

When given a symbolic boolean value, however, a Rosette assertion has no immediate effect.  Instead, the value is accumulated in the current @tech[#:key "vc"]{verification condition} (VC), and the assertion's effect (whether it passes or fails) is eventually determined by the solver.

@interaction[#:eval rosette-eval
(code:line (spec-asserts (vc)) (code:comment "We asserted #f above, so the current VC reflects that."))
(code:line (clear-vc!)         (code:comment "Clear the current VC."))
(spec-asserts (vc))
(code:line (assert (not b))    (code:comment "Add the assertion (not b) to the VC."))
(spec-asserts (vc))
(clear-vc!)]

Assertions express properties that a program must satisfy on all @emph{legal} inputs. In Rosette, as in other solver-aided frameworks, we use @emph{assumptions} to describe which inputs are legal. If a program violates an assertion on a legal input, we blame the program. But if it violates an assertion on an illegal input, we blame the caller. In other words, a program is considered incorrect only when it violates an assertion on a legal input.

Assumptions behave analogously to assertions on both concrete and symbolic values. In the concrete case, assuming @racket[#f] aborts the execution with a runtime exception, and assuming a true value is equivalent to calling @racket[(void)]. In the symbolic case, the assumed value is accumulated in the current VC.

@interaction[#:eval rosette-eval
(assume #t)
(spec-assumes (vc))
(code:line (assume #f)          (code:comment "Assuming #f aborts the execution with an exception."))
(spec-assumes (vc))
(clear-vc!)
(define-symbolic i j integer?)
(code:line (assume (> j 0))     (code:comment "Add the assumption (> j 0) to the VC."))
(spec-assumes (vc))
(assert (< (- i j) i))
(code:line (spec-asserts (vc))  (code:comment "The assertions must hold when the assumptions hold"))
(code:line (pretty-print (vc))  (code:comment "VC tracks the assumptions and the assertions."))]

@(rosette-eval '(clear-vc!))

@section[#:tag "sec:queries"]{Solver-Aided Queries}

The solver reasons about assumed and asserted properties only when we ask a question about them---for example, "Does my program have an execution that violates an assertion while satisfying all the assumptions?"  We pose such @emph{solver-aided queries} with the help of constructs explained in the remainder of this chapter.

We will illustrate the queries on the following toy example. Suppose that we want to implement a
procedure @racket[bvmid] that takes as input two non-negative 32-bit integers, @racket[lo] ≤ @racket[hi],
and returns the midpoint of the interval [@racket[lo], @racket[hi]]. In C or Java, we would declare
the inputs and output of @racket[bvmid] to be of type ``int''. In Rosette, we model finite precision
(i.e., machine) integers as @seclink["sec:bitvectors"]{bitvectors} of length 32.

@defs+int[#:eval rosette-eval
((code:comment "int32? is a shorthand for the type (bitvector 32).")
 (define int32? (bitvector 32))

 (code:comment "int32 takes as input an integer literal and returns")
 (code:comment "the corresponding 32-bit bitvector value.")
 (define (int32 i)
   (bv i int32?)))

 (code:line (int32? 1)         (code:comment "1 is not a 32-bit integer"))
 (code:line (int32? (int32 1)) (code:comment "but (int32 1) is."))
 (int32 1)]

Bitvectors support the usual operations on machine integers, and we can use them to implement @racket[bvmid] as follows: 

@defs+int[#:eval rosette-eval
((code:comment "Returns the midpoint of the interval [lo, hi].") 
 (define (bvmid lo hi)              
  (bvsdiv (bvadd lo hi) (int32 2))) (code:comment "(lo + hi) / 2"))]

As the above implementation suggests, we intend the midpoint to be the mathematical
integer @tt{mi = (lo + hi) / 2}, where @tt{/} stands for integer division. Assuming
that @tt{0 ≤ lo ≤ hi}, the midpoint @tt{mi} is fully characterized by two properties:
(1) @tt{lo ≤ mi ≤ hi} and (2) @tt{0 ≤ (hi - mi) - (mi - lo) ≤ 1}. We can use these
properties to define a generic correctness specification for any implementation of
@racket[bvmid] as follows: 

@defs+int[#:eval rosette-eval
((define (check-mid impl lo hi)     (code:comment "Assuming that")
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

@interaction[#:eval rosette-eval
 (check-mid bvmid (int32 0) (int32 0))
 (check-mid bvmid (int32 0) (int32 1))
 (check-mid bvmid (int32 0) (int32 2))
 (check-mid bvmid (int32 10) (int32 10000)) ]

But does it work correctly on @emph{all} legal inputs?  The answer, as we will see below, is ``no''.
In fact, @racket[bvmid] reproduces  @hyperlink["https://en.wikipedia.org/wiki/Binary_search_algorithm#Implementation_issues"]{a famous bug}
that lurked for years in widely used C and Java implementations of binary search.
       

@subsection[#:tag "sec:verify"]{Verification}

How can we check if @racket[bvmid] satisfies its specification on all legal inputs? One approach is to enumerate all pairs of 32-bit integers with @racket[0 ≤ lo ≤ hi] and apply @racket[(check-mid bvmid hi lo)] to each. This approach is sound (it is guaranteed to find a bug if one exists), but a quick calculation shows that it is impractical even for our toy example: @racket[bvmid] has roughly 2.3 × 10@superscript{18} legal inputs. A better approach is to delegate such checks to a constraint solver, which can search  large input spaces much more effectively than naive enumeration.  In Rosette, this is done with the help of the @racket[verify] query:

@interaction[#:eval rosette-eval
(define-symbolic l h int32?)
(define cex (verify (check-mid bvmid l h)))
cex] 

The @racket[(verify #, @var[expr])] form queries the solver for a @deftech{binding} from symbolic constants to concrete values that causes the evaluation of @var[expr] to violate an assertion, while satisfying all the assumptions, when the bound symbolic constants are replaced with the corresponding concrete values. If such a binding exists, as it does in our case, it is called a @emph{counterexample}. 

Bindings are first-class values in Rosette, and they can be freely manipulated by programs.  We can also interpret any Rosette value with respect to a binding using the built-in @racket[evaluate] procedure:
@interaction[#:eval rosette-eval
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
(check-mid bvmid cl ch)]
@(rosette-eval '(clear-vc!))

In our example, evaluating @racket[l] and @racket[h] with respect to @racket[cex] reveals that @racket[bvmid] fails to return the correct midpoint value, thus causing the first assertion in the @racket[check-mid] procedure to fail. The bug is due to overflow:  
the expression @racket[(bvadd lo hi)] in @racket[bvmid] produces a negative value in
the 32-bit representation when the sum of
@racket[lo] and @racket[hi] exceeds 2@\superscript{31}-1.

@interaction[#:eval rosette-eval
(bvadd cl ch)
(bitvector->integer (bvadd cl ch))
(+ il ih)
(- (expt 2 31) 1)]
             
@(rosette-eval '(clear-vc!))

One @hyperlink["https://en.wikipedia.org/wiki/Binary_search_algorithm#Implementation_issues"]{solution}
to this problem is to calculate the midpoint as @tt{lo + ((hi - lo) / 2)}. It is easy to see that all
intermediate values in this calculation are at most @racket[hi] when @racket[lo] and @racket[hi] are both non-negative, so no overflow can happen. We can also verify this with Rosette:

@def+int[#:eval rosette-eval
(define (bvmid-no-overflow lo hi)
  (bvadd lo (bvsdiv (bvsub hi lo) (int32 2))))

(verify (check-mid bvmid-no-overflow l h))]

@subsection[#:tag "sec:synthesize"]{Synthesis}


The correct solution, like our buggy one, relies on signed division by 2, creating a potential opportunity for optimization. In general, this division cannot be replaced by a shifting operation, but is there be a way to do so in our example? To find out, we can, once again, ask the solver for help---this time, via the @racket[synthesize] query.

The synthesis query uses the solver to search for a correct program in a space of candidate implementations defined by a syntactic @deftech{sketch}. A sketch is a program with @deftech[#:key "hole"]{holes}, which the solver fills with expressions drawn from a specified set of options. For example, @racket[(?? int32?)] stands for a hole that can be filled with any 32-bit integer constant, so the sketch @racket[(bvadd x (?? int32?))] represents all 2@superscript{32} programs that add a 32-bit constant to the variable @racket[x]. Rosette also lets you define richer holes that can be filled with expressions from a given grammar. For example, here is a grammar of all @racket[int32?] expressions that consist of cheap arithmetic and bitwise operations:

@defs+int[#:eval rosette-eval
((require rosette/lib/synthax)     (code:comment "Require the sketching library.")
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

@defs+int[#:eval rosette-eval
((define (bvmid-fast lo hi)
  (fast-int32 lo hi #:depth 2)))]

The above sketch describes the space of all expressions from the @racket[fast-int32] grammar that have parse trees of depth at most 2. The depth argument is optional. If ommitted, Rosette will use the value of the @racket[(current-grammar-depth)] parameter to bound the depth of the expressions drawn from the grammar.

At this point, it is worth noting that holes and sketches are not fundamental concepts in Rosette. Instead, they are macros defined in terms of the core constructs we have already seen, symbolic constants and assertions. For example,  @racket[(?? int32?)] is syntactic sugar for @racket[(let () (define-symbolic #, @var[id] int32?) #, @var[id])], where @var[id] is an internally generated name. Similarly, @racket[(choose bvneg bvnot)] expands to @racket[(if (?? boolean?) bvneg bvnot)]. Finally, a grammar hole such as @racket[(fast-int32 lo hi #:depth 2)] inlines its productions @racket[#:depth] times to create a nested expression of the form @racket[(choose lo hi (?? int32?) ((choose bvadd ...) (choose ...) (choose ...)) ((choose bvneg bvnot) (choose ...)))]. Assigning concrete values to the symbolic constants generated by this expression has the effect of selecting a parse tree (of depth 2 in our example) from the hole's grammar. So, completing a sketch is a matter of finding a suitable binding for the symbolic constants generated by the holes.

With this in mind, we can query the solver for a completion of the @racket[bvmid-fast] sketch (if any) that satisfies our correctness specification:
@interaction[#:eval rosette-eval
(code:comment "Save the above definitions to a file before calling print-forms.")
(define sol
  (synthesize
   #:forall    (list l h)
   #:guarantee (check-mid bvmid-fast l h)))
(eval:alts
 sol
 "(model
 [0$choose:bvmid:49:9$expr:bvmid:49:3$fast-int32:bvmid:57:3 #f]
 [1$choose:bvmid:49:9$expr:bvmid:49:3$fast-int32:bvmid:57:3 #f]
 [2$choose:bvmid:49:9$expr:bvmid:49:3$fast-int32:bvmid:57:3 #f]
 [3$choose:bvmid:49:9$expr:bvmid:49:3$fast-int32:bvmid:57:3 #t]
 ...)")
(eval:alts 
(print-forms sol)
'(define (bvmid-fast lo hi) (bvlshr (bvadd hi lo) (bv #x00000001 32))))]

The synthesis query takes the form @racket[(synthesize #:forall #, @var[input] #:guarantee #, @var[expr])], where @var[input] lists the symbolic constants that represent inputs to a sketched program, and @var[expr] gives the correctness specification for the sketch. The solver searches for a binding from the hole (i.e., non-@var[input]) constants to values such that @var[expr] satisfies its assertions on all legal @var[input]s. Passing this binding to @racket[print-forms] converts it to a syntactic representation of the completed sketch.@footnote{@racket[print-forms] works only on sketches that have been saved to disk.} In our example, the synthesized program implements the midpoint calculation using the logical shift operation, i.e., the midpoint between @racket[lo] and @racket[hi] can be calculated as @tt{(lo + hi) >>@subscript{u} 1}.

@subsection[#:tag "sec:solve"]{Angelic Execution}

Rosette supports one more solver-aided query, which we call "angelic execution." This query is the opposite of verification.  Given a program with symbolic values, it instructs the solver to find a binding for them that will cause the program to execute successfully---that is, without any assertion failures. 

Angelic execution can be used to solve puzzles, to run incomplete code, or to "invert" a program, by searching for inputs that produce a desired output.  For example, we can ask the solver to find two distinct input values, which are not zeros of the @racket[poly] function, but which @racket[poly] still maps to the same output: 
@interaction[#:eval rosette-eval
(define-symbolic x y integer?)
(define sol 
  (solve (begin (assert (not (= x y)))
                (assert (< (abs x) 10))
                (assert (< (abs y) 10))
                (assert (not (= (poly x) 0)))
                (assert (= (poly x) (poly y))))))
(evaluate x sol)
(evaluate y sol)
(evaluate (poly x) sol)
(evaluate (poly y) sol)]

You can find more examples of angelic execution and other solver-aided queries in the @hyperlink["https://github.com/emina/rosette/blob/master/sdsl/"]{@racket[sdsl]} folder of your Rosette distribution.

@(rosette-eval '(clear-vc!))

@section[#:tag "sec:notes"]{Symbolic Reasoning}

Rosette implements solver-aided queries by translating them to the input language of an SMT solver.
This translation is performed using a given @tech["reasoning precision"], as specified
by the @racket[current-bitwidth] parameter.  Setting @racket[current-bitwidth]
to a positive integer @var{k} instructs Rosette to approximate both reals and integers with @var{k}-bit words.
Setting it to @racket[#f] instructs Rosette to use infinite precision for real and integer operations.

The following snippet shows the effect of different @racket[current-bitwidth] settings on query behavior:
@interaction[#:eval rosette-eval
(define-symbolic x integer?)
(current-bitwidth 5)  (code:comment "64 = 0 in the 5-bit representation")
(solve (begin (assert (= x 64))
              (assert (= x 0))))
(verify (assert (not (and (= x 64) (= x 0))))) 
(current-bitwidth #f) (code:comment "but no solutions exist under infinite-precision semantics")
(solve (begin (assert (= x 64))
              (assert (= x 0))))
(verify (assert (not (and (= x 64) (= x 0)))))]

By default, @racket[current-bitwidth] is set to @racket[#f] to be consistent with Racket's
infinite-precision semantics for integers and reals. Beware, however, that using @racket[#f] or a large @var{k}
for @racket[current-bitwidth] may have a negative effect on solver performance.  In the worst case, using @racket[#f] can cause the underlying solver to run forever.@footnote{Technically, Rosette translates solver-aided queries to the theory of bitvectors when @racket[current-bitwidth] is set to an integer @var{k}. In particular, it uses @var{k}-bit bitvectors to represent integers and reals, with smaller bitvectors leading to better performance.  When @racket[current-bitwidth] is @racket[#f], Rosette uses the theories of integers and reals instead.  These theories work well for linear constraints, but reasoning about non-linear integer arithmetic is undecidable.}
                                                               
Non-termination can also be caused by passing symbolic values to recursive procedures.  In particular, the expression that determines whether a recursion (or a loop) terminates must be executed on concrete values.   

@interaction[
(code:comment "while sandboxed evaluation of (ones x) times out,")
(code:comment "normal evaluation would not terminate")
(eval:alts (define-symbolic x integer?) (void))
(eval:alts 
(letrec ([ones (lambda (n)
                  (if (<= n 0)
                      (list)
                      (cons 1 (ones (- n 1)))))])
  (printf "~a" (ones 3))
  (printf "~a" (ones x)))
(begin (printf "(1 1 1)")
       (fprintf (current-error-port) "with-limit: out of time")))]


It is, however, safe to apply recursive procedures to symbolic values if they are not used in termination checks.  
@interaction[#:eval rosette-eval
(define-symbolic x integer?)
(letrec ([adder (lambda (vs n)
                  (if (null? vs)
                      (list)
                      (cons (+ (car vs) n) (adder (cdr vs) n))))])
  (adder '(1 2 3) x))]

See Chapters @seclink["ch:symbolic-reflection"]{7} and @seclink["ch:unsafe"]{8} to
learn more about common patterns and anti-patterns for effective symbolic reasoning.

@(kill-evaluator rosette-eval)


@(footnote-part)
