#lang scribble/manual

@(require (for-label racket)
          (for-label  
           rosette/base/form/define
           (only-in rosette/base/base assert assume vc spec-asserts spec-assumes clear-vc!)
           rosette/query/query  
           (only-in rosette/base/base bv?)
           rosette/query/eval
           rosette/lib/synthax))

@(require racket/sandbox  racket/runtime-path  scribble/core
          scribble/eval scribble/html-properties ;scriblib/footnote 
          (only-in racket [unsyntax racket/unsyntax])
          (only-in racket/draw read-bitmap))

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
  (define-symbolic x boolean?) (code:comment "creates the same constant when evaluated")
  x)

(define (dynamic) 
  (define-symbolic* y integer?) (code:comment "creates a different constant when evaluated")
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
(code:line (spec-asserts (vc)) (code:comment "we asserted #f above, so the assert in the current VC is #f"))
(code:line (clear-vc!)         (code:comment "clear the current VC"))
(spec-asserts (vc))
(code:line (assert (not b))    (code:comment "assert (not b) in the VC"))
(spec-asserts (vc))
(clear-vc!)]

Assertions express properties that a program must satisfy on all @emph{legal} inputs. In Rosette, as in other solver-aided frameworks, we use @emph{assumptions} to describe which inputs are legal. If a program violates an assertion on a legal input, we blame the program. But if it violates an assertion on an illegal input, we blame the caller. In other words, a program is considered incorrect only when it violates an assertion on a legal input.

Assumptions behave analogously to assertions on both concrete and symbolic values. In the concrete case, assuming @racket[#f] aborts the execution with a runtime exception, and assuming a true value is equivalent to calling @racket[(void)]. In the symbolic case, the assumed value is accumulated in the current VC.

@interaction[#:eval rosette-eval
(assume #t)
(spec-assumes (vc))
(code:line (assume #f)          (code:comment "assuming #f aborts the execution with an exception"))
(spec-assumes (vc))
(clear-vc!)
(define-symbolic i j integer?)
(code:line (assume (> j 0))     (code:comment "assume (> j 0) in the VC"))
(spec-assumes (vc))
(assert (< (- i j) i))
(code:line (spec-asserts (vc))  (code:comment "the assertions must hold when the assumptions hold"))
(code:line (pretty-print (vc))  (code:comment "VC tracks the assumptions and the assertions"))]

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
that @tt{0 ≤ lo ≤ hi}, the midpoint @tt{mi} is fully characterized by the property 
@tt{0 ≤ (hi - mi) - (mi - lo) ≤ 1}. We can use this property to define a generic 
correctness specification for any implementation of @racket[bvmid] as follows: 

@defs+int[#:eval rosette-eval
((define (check-mid impl lo hi)      (code:comment "Assuming that")
   (assume (bvsle (int32 0) lo))      (code:comment "0 ≤ lo and")             
   (assume (bvsle lo hi))             (code:comment "lo ≤ hi, ") 
   (define mi (impl lo hi))           (code:comment "and letting mi = impl(lo, hi) and") 
   (define diff                       (code:comment "diff = (hi - mi) - (mi - lo),")
     (bvsub (bvsub hi mi)             
            (bvsub mi lo)))           (code:comment "we require that")
   (assert (bvsle (int32 0) diff))    (code:comment "0 ≤ diff and")
   (assert (bvsle diff (int32 1))))   (code:comment "diff ≤ 1."))]

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
(define cex (verify (check-mid bvmid l h)))] 

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

In our example, evaluating @racket[l] and @racket[h] with respect to @racket[cex] reveals that @racket[bvmid] fails to return the correct midpoint value, thus causing the assertion in the @racket[check-mid] procedure to fail. The bug is due to overflow:  
the expression @racket[(bvadd lo hi)] in @racket[bvmid] produces a negative value in
the 32-bit representation when the sum of
@racket[lo] and @racket[hi] exceeds 2@\superscript{31}-1.

@interaction[#:eval rosette-eval
(bvadd cl ch)
(bitvector->integer (bvadd cl ch))
(+ il ih)
(- (expt 2 31) 1)]
             
@(rosette-eval '(clear-vc!))



@subsection[#:tag "sec:synthesize"]{Synthesis}

The solver can not only find failure-inducing inputs, it can also synthesize repairs for buggy expressions.  To repair a program, we first replace each buggy expression with a syntactic "@deftech{hole}."  A program with holes is called a @deftech{sketch}.  The solver completes a sketch by filling its holes with expressions, in such a way that all assertions in the resulting program pass on all inputs.  

The following code snippet shows the sketch for our buggy @racket[factored] procedure. The sketch reflects our knowledge that a factorization of an @var{n}-degree polynomial takes the form @tt{(* (+ x @var[c]@subscript{0}) ... (+ x @var[c]@subscript{@var{n}}))}, where @var[c]@subscript{@var{i}} is an integer constant. We ask the solver to find these constants by leaving holes @racket[(??)] in the program.

@defs+int[#:eval rosette-eval
((require rosette/lib/synthax) 
 
 (define (poly x)
  (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))

 (define (factored x)        
  (* (+ x (??)) (+ x (??)) (+ x (??)) (+ x (??))))  
 
 (define (same p f x)
  (assert (= (p x) (f x)))))]

The @racket[(??)] construct is imported from the @racket[rosette/lib/synthax] library, which also provides constructs for specifying more complex holes.  For example, you can specify a hole that is filled with an expression, drawn from a grammar you define.   


We query the solver for a correct completion of our sketch as follows:
@interaction[#:eval rosette-eval
(code:comment "Call after saving the above definitions to a file:")                    
(define-symbolic i integer?)
(define binding 
  (synthesize #:forall (list i)
              #:guarantee (same poly factored i)))
(eval:alts (print-forms binding) '(define (factored x) (* (+ x 1) (+ x 3) (+ x 2) (+ x 0))))]
The @racket[(synthesize #:forall #, @var[input] #:guarantee #, @var[expr])] query uses the @var[input] form to specify a set of distinguished symbolic values, which are treated as inputs to the expression @var[expr]. The result, if any, is a binding for the remaining symbolic values, created by evaluating holes.  This binding guarantees successful evaluation of @var[expr] for @emph{all} possible bindings of the @var[input] values. Passing it to the @racket[print-forms] procedure yields a syntactic representation of the completed sketch.@footnote{@racket[print-forms] can only print the completion of a sketch that has been saved to a file.}

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
