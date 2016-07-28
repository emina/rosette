#lang scribble/manual

@(require (for-label racket)
          (for-label  
           rosette/base/form/define (only-in rosette/base/core/safe assert)
           rosette/query/query (only-in rosette asserts clear-asserts!)
           (only-in rosette/base/base bv?)
           (except-in rosette/query/debug assert) rosette/query/eval
           (only-in rosette/lib/synthax ?? print-forms) rosette/lib/render))

@(require racket/sandbox  racket/runtime-path  scribble/core
          scribble/eval scribble/html-properties ;scriblib/footnote 
          (only-in racket [unsyntax racket/unsyntax])
          (only-in racket/draw read-bitmap))

@(require (only-in "../refs.scrbl" ~cite rosette:onward13 rosette:pldi14)
          "../util/lifted.rkt")
 
@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "essentials-log")))

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
that can answer interesting questions about program behaviors.  They are based on three 
key concepts: @emph{symbolic values}, @emph{assertions} and @emph{queries}. 
We use assertions to express desired program behaviors and symbolic values to 
formulate queries about these behaviors.

This chapter illustrates the basics of solver-aided programming with a 
few simple examples. More advanced tutorials, featuring extended examples, can be found 
in Section 2 of @~cite[rosette:onward13 rosette:pldi14].@footnote{Code examples in  
these references are written in Rosette 1.0.
While Rosette 2.0 is not backward compatible with Rosette 1.0,
they share the same conceptual core.} 

The following chapters describe the subset 
of Racket that can be @seclink["sec:langs"]{safely} used with solver-aided facilities, including the 
supported datatypes (both @seclink["ch:built-in-datatypes"]{built-in}
and @seclink["ch:programmer-defined-datatypes"]{programmer-defined}), 
@seclink["ch:syntactic-forms"]{syntactic forms}, and @seclink["ch:libraries"]{libraries}. 

@section[#:tag "sec:symbolic-values"]{Symbolic Values}

The Rosette language includes two kinds of values: concrete and symbolic.  Concrete values are plain Racket values (@racket[#t], @racket[#f], @racket[0], @racket[1], etc.), and Rosette programs that operate only on concrete values behave just like Racket programs.  Accessing the solver-aided features of Rosette---such as code synthesis or verification---requires the use of symbolic values.

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
    

@section[#:tag "sec:asserts"]{Assertions}

Like many other languages, Rosette provides a construct for expressing @emph{assertions}---important properties of programs that are checked in every execution.  Rosette assertions work just like Java or Racket assertions when given a concrete value:  if the value is false, the execution terminates with a runtime error.  Otherwise, the execution proceeds normally.  
@interaction[#:eval rosette-eval
(assert #t) 
(assert #f)]

When given a symbolic boolean value, however, a Rosette assertion has no immediate effect.  Instead, its effect (whether it passes or fails) is eventually determined by the solver.
@interaction[#:eval rosette-eval
(code:comment "add (not b) to the stack of assertions to be solved")
(assert (not b))
(code:comment "retrieve the assertion store")
(asserts)
(code:comment "clear the assertion store")
(clear-asserts!)
(asserts)]

@(rosette-eval '(clear-asserts!))

@section[#:tag "sec:queries"]{Solver-Aided Queries}

The solver reasons about asserted properties only when we ask a question about them---for example, "Does my program have an execution that violates an assertion?"  We pose such @emph{solver-aided queries} with the help of constructs explained in the remainder of this chapter.

We will illustrate the queries on the following toy example, where the @racket[factored] polynomial is intended to behave just like @racket[poly] on all inputs:
@defs+int[#:eval rosette-eval
((define (poly x)
  (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))

 (define (factored x)
  (* x (+ x 1) (+ x 2) (+ x 2)))
 
 (define (same p f x)
  (assert (= (p x) (f x)))))

(code:comment "check zeros; all seems well ...")
(same poly factored 0)
(same poly factored -1)
(same poly factored -2)]
       

@subsection[#:tag "sec:verify"]{Verification}

To verify that @racket[poly] and @racket[factored] behave identically, we could simply enumerate all k-bit integers and apply the @racket[same] check to each.  This naive approach to verification would, of course, be very slow for a large k---and impossible for infinite precision integers. A better approach is to delegate such checks to a constraint solver, which can search large input spaces more effectively.  In Rosette, this is done with the help of the @racket[verify] query:
@interaction[#:eval rosette-eval
(define-symbolic i integer?)
(define cex (verify (same poly factored i)))] 

The @racket[(verify #, @var[expr])] form queries the solver for a @deftech{binding} from symbolic constants to concrete values that causes the evaluation of @var[expr] to fail when the bound symbolic constants are replaced with the corresponding concrete values. If such a binding exists, as it does in our case, it is called a @emph{counterexample}. 

Bindings are first-class values in Rosette, and they can be freely manipulated by programs.  We can also interpret any Rosette value with respect to a binding using the built-in @racket[evaluate] procedure:
@interaction[#:eval rosette-eval
(evaluate i cex) 
(same poly factored 12)]
In our example, evaluating @racket[i] with respect to @racket[cex] reveals that @racket[poly] and @racket[factored] produce different results on the input 12 (thus causing the assertion in the @racket[same] procedure to fail).

@(rosette-eval '(clear-asserts!))
@(rosette-eval '(require (only-in racket/draw read-bitmap)))

@subsection[#:tag "sec:debug"]{Debugging}

Now that we have an input on which @racket[factored] differs from @racket[poly], the next step is to debug it, by figuring out which of its subexpressions are responsible for the fault.  Rosette provides a query for this as well.  To access it, we import the debugging facilities, mark @racket[factored] as a candidate for debugging, and issue a @racket[debug] query: 

@racketblock[
(require rosette/query/debug rosette/lib/render)
 
(define (poly x)
  (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))

(define/debug (factored x)       (code:comment "define/debug marks a procedure as part of")
  (* x (+ x 1) (+ x 2) (+ x 2))) (code:comment "the code to be debugged")
 
(define (same p f x)
  (assert (= (p x) (f x))))

(code:comment "Call after saving the above definitions to a file:")
#, @elem{>} (define ucore (debug [integer?] (same poly factored 12)))
#, @elem{>} (render ucore) 
#,(call-with-input-file (build-path root "pict.png") (lambda (in) (read-bitmap in 'png)))]

@(rosette-eval '(require rosette/query/debug))
@(rosette-eval '(define (poly x)
       (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x))))
@(rosette-eval '(define/debug (factored x)        
       (* x (+ x 1) (+ x 2) (+ x 2))))
@(rosette-eval '(define (same p f x)
       (assert (= (p x) (f x)))))
@(rosette-eval '(define ucore (debug [integer?] (same poly factored 12))))

The @racket[(debug [#, @var[predicate]] #, @var[expr])] query takes as input an expression whose execution leads to an assertion failure, and one or more dynamic type predicates specifying which executed expressions should be treated as potentially faulty by the solver. That is, the predicates express the hypothesis that the failure is caused by an expression with one of the given types. Expressions that produce values of a different type are assumed to be correct.@footnote{For now, only primitive (@racket[boolean?], @racket[integer?], @racket[real?], and @racket[bv?]) types can be used in @racket[debug] forms.}

The output of a @racket[debug] query is a minimal set of program expressions, called a @deftech[#:key "MUC"]{minimal unsatisfiable core}, that form an irreducible cause of the failure. Expressions outside of the core are irrelevant to the failure---there is no way to replace them with constants so that the resulting program satisfies the failing assertion. The failing assertion can only be satisfied if we are allowed to also replace one of the core expressions with a carefully chosen constant.  In general, a failing expression may have many different cores, but since every core highlights a buggy subexpression, examining one or two cores often leads to the root cause of the error.

Like bindings, cores are first-class values. In our example, we simply visualize the core using the utility procedure @racket[render].@footnote{@racket[render] can only visualize cores for code that has been saved to a file.} The visualization reveals that the grayed-out subexpression @racket[(+ x 1)] is irrelevant to the failure of @racket[factored] on the input 12.  To repair this failure, we have to modify at least one of the remaining expressions, which are highlighted in red.  

@subsection[#:tag "sec:synthesize"]{Synthesis}

The solver can not only find failure-inducing inputs and localize faults, it can also synthesize repairs for buggy expressions.  To repair a program, we first replace each buggy expression with a syntactic "@deftech{hole}."  A program with holes is called a @deftech{sketch}.  The solver completes a sketch by filling its holes with expressions, in such a way that all assertions in the resulting program pass on all inputs.  

The following code snippet shows the sketch for our buggy @racket[factored] procedure.  We obtained it by replacing the constants in the @seclink["sec:debug"]{minimal core} with @racket[(??)] holes, which are filled with numerical constants.@footnote{This simple replacement strategy is sufficient since we know that a factorization of an @var{n}-degree polynomial takes the form @tt{(* (+ x @var[c]@subscript{0}) ... (+ x @var[c]@subscript{@var{n}}))}, where @var[c]@subscript{@var{i}} is a constant.}
@defs+int[#:eval rosette-eval
((require rosette/lib/synthax) 
 
 (define (poly x)
  (+ (* x x x x) (* 6 x x x) (* 11 x x) (* 6 x)))

 (define (factored x)        
  (* (+ x (??)) (+ x 1) (+ x (??)) (+ x (??))))  
 
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
(eval:alts (print-forms binding) '(define (factored x) (* (+ x 0) (+ x 1) (+ x 2) (+ x 3))))]
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

@(rosette-eval '(clear-asserts!))

@section[#:tag "sec:notes"]{Symbolic Reasoning}

Rosette implements solver-aided queries by translating them to the input language of an SMT solver.
This translation is performed using a given @tech["reasoning precision"], as specified
by the @racket[current-bitwidth] parameter.  Setting @racket[current-bitwidth]
to a positive integer @var{k} instructs Rosette to approximate both reals and integers with @var{k}-bit words.
Setting it to @racket[#f] instructs Rosette to use infinite precision for real and integer operations.

The following snippet shows the effect of different @racket[current-bitwdth] settings on query behavior:
@interaction[#:eval rosette-eval
(define-symbolic x integer?)
(current-bitwidth 5)  (code:comment "no 5-bit solution or counterexample exists")
(solve (assert (= x 64)))
(verify (assert (not (= x 64)))) 
(current-bitwidth #f) (code:comment "but an integer solution and counterexample do exist")
(solve (assert (= x 64)))
(verify (assert (not (= x 64))))]

By default, @racket[current-bitwidth] is set to 5.  Beware that using a large @var{k} or @racket[#f]
 may have a negative effect on solver performance.  In the worst case, using @racket[#f] can cause the underlying solver to run forever.@footnote{Technically, Rosette translates solver-aided queries to the theory of bitvectors when @racket[current-bitwidth] is set to an integer @var{k}. In particular, it uses @var{k}-bit bitvectors to represent integers and reals, with smaller bitvectors leading to better performance.  When @racket[current-bitwidth] is @racket[#f], Rosette uses the theories of integers and reals instead.  These theories work well for linear constraints, but reasoning about non-linear integer arithmetic is undecidable.}
                                                               
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