#lang scribble/manual

@(require (for-label  
           rosette/base/form/define rosette/query/form rosette/query/eval rosette/solver/solution
           rosette/base/core/term (only-in rosette/query/debug define/debug debug)
           (only-in rosette/query/finitize current-bitwidth)
           (only-in rosette/base/core/safe assert) 
           (only-in rosette/base/core/bool asserts clear-asserts!)
           (only-in rosette/base/base bv?)
           (only-in rosette/base/core/function function?))
          (for-label racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox  racket/runtime-path 
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "rosette-forms-log")))

@title[#:tag "ch:syntactic-forms:rosette"]{Solver-Aided Forms}

The @seclink["ch:essentials"]{Essentials} chapter introduced the key concepts of solver-aided programming.  This section defines the corresponding syntactic constructs more precisely.

@declare-exporting[rosette/base/form/define 
                   rosette/query/form 
                   rosette/base/core/safe
                   rosette/base/core/bool
                   rosette/query/finitize
                   #:use-sources 
                   (rosette/base/form/define 
                   rosette/query/form
                   rosette/base/core/safe
                   rosette/base/core/bool
                   rosette/query/finitize)]

@section[#:tag "sec:symbolic-constants"]{Symbolic Constants}

@defform[(define-symbolic id ...+ type)
         #:contracts
         [(type (and/c solvable? type?))]]{
  Binds each provided identifier to a distinct @tech["symbolic constant"] of the given 
  @tech["solvable type"].  The identifiers are bound to the same constants every time the form is 
  evaluated.  
  @examples[#:eval rosette-eval
  (define (always-same)
    (define-symbolic x integer?)
    x)
  (always-same)
  (always-same) 
  (eq? (always-same) (always-same))]
}
@defform[(define-symbolic* id ...+ type)
         #:contracts
         [(type (and/c solvable? type?))]]{
  Creates a stream of distinct @tech["symbolic constant"] of the given 
  @tech["solvable type"] for each identifier, binding the identifier to the 
  next element from its stream every time the form is evaluated.  
  @examples[#:eval rosette-eval
  (define (always-different)
    (define-symbolic* x integer?)
    x)
  (always-different) 
  (always-different) 
  (eq? (always-different) (always-different))]
}

@section[#:tag "sec:assertions"]{Assertions}

@defform[(assert expr maybe-message)
         #:grammar
         [(maybe-message (code:line) msg)]
         #:contracts
         [(msg (or/c string? procedure?))]]{
  Provides a mechanism for communicating desired
  program properties to the underlying solver.  Rosette keeps track of all
  assertions evaluated during an execution in an @tech{assertion store}. 
  If @racket[expr] evaluates to @racket[#f], an error is thrown using the 
  optional failure message, and @racket[#f] is added to the assertion store.  The error message
  can be either a string or a no-argument procedure that throws an error when called.
  If @racket[expr] evaluates to a symbolic boolean value, that value is added to the assertion store.
  If @racket[expr] evaluates to any other value, @racket[assert] has no effect.  The contents
  of the assertion store can be examined using the @racket[asserts] procedure, and they can be
  cleared using the @racket[clear-asserts!] procedure. 
  @examples[#:eval rosette-eval
  (code:line (assert #t) (code:comment "no effect"))
  (code:line (assert 1)  (code:comment "no effect"))
  (code:line (asserts)   (code:comment "retrieve the assertion store"))  
  (define-symbolic x boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x added to the assertion store"))  
  (assert #f "bad value")
  (asserts) 
  (code:line (clear-asserts!)   (code:comment "clear the assertion store"))
  (asserts)]
}

@section{Angelic Execution}

@defform[(solve expr)]{
  Searches for a binding of symbolic constants to concrete values that satisfies all assertions encountered
  before the invocation of @racket[solve] and during the evaluation of @racket[expr]. 
  If such a binding exists, it is returned in the form of a satisfiable @racket[solution?]; otherwise, 
  the result is an unsatisfiable solution.  The assertions encountered while 
  evaluating @racket[expr] are removed from the global @tech["assertion store"] once @racket[solve] returns.  As a result, 
  @racket[solve] has no observable effect on the @tech["assertion store"]. 
  The solver's ability to find solutions depends on the current @tech["reasoning precision"], as
  determined by the @racket[current-bitwidth] parameter.
  @examples[#:eval rosette-eval
  (define-symbolic x y boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x added to the assertion store"))  
  (define sol (solve (assert y)))
  (code:line (asserts)   (code:comment "assertion store same as before")) 
  (code:line (evaluate x sol) (code:comment "x must be true"))
  (code:line (evaluate y sol) (code:comment "y must be true"))
  (solve (assert (not x)))]
  @;We refer to the  
  @;@racket[solve] query as @deftech{angelic execution} because it causes the solver to behave as an
  @;angelic oracle---it supplies "good" bindings for symbolic constants that cause the execution to terminate successfully.
}

@(rosette-eval '(clear-asserts!))

@section{Verification}

@defform*[((verify guarantee-expr)
           (verify #:assume assume-expr #:guarantee guarantee-expr))]{
  Searches for a binding of symbolic constants to concrete values that violates at least one of the 
  assertions encountered during the evaluation of @racket[guarantee-expr], but that satisfies all 
  assertions encountered before the invocation of @racket[verify] and during the evaluation of 
  @racket[assume-expr]. If such a binding exists, it is returned in the form of a
  satisfiable @racket[solution?]; otherwise, the result is an unsatisfiable solution.  The assertions encountered while 
  evaluating @racket[assume-expr] and @racket[guarantee-expr] are removed from the global @tech["assertion store"] once 
  @racket[verify] returns.   
  The solver's ability to find solutions depends on the current @tech["reasoning precision"], as
  determined by the @racket[current-bitwidth] parameter.
  @examples[#:eval rosette-eval
  (define-symbolic x y boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x added to the assertion store")) 
  (define sol (verify (assert y)))
  (code:line (asserts)   (code:comment "assertion store same as before")) 
  (code:line (evaluate x sol) (code:comment "x must be true"))
  (code:line (evaluate y sol) (code:comment "y must be false"))
  (verify #:assume (assert y) #:guarantee (assert (and x y)))]
}

@(rosette-eval '(clear-asserts!))

@section{Synthesis}

@defform[(synthesize
            #:forall input-expr
            maybe-assume
            #:guarantee guarantee-expr)
          #:grammar ([maybe-assume (code:line) (code:line #:assume assume-expr)])
          #:contracts [(input-expr (listof constant?))]]{
  Searches for a binding of symbolic constants 
  to concrete values that has the following properties: 
  @itemlist[#:style 'ordered
  @item{it does not map constants in the @racket[input-expr] list; and,} 
  @item{it satisfies all assertions encountered during the evaluation of 
  @racket[guarantee-expr], for every binding of @racket[input-expr] constants to values that satisfies 
  the assertions encountered before the invocation of @racket[synthesize] and during the evaluation of 
  @racket[assume-expr].}] 
  If no such binding exists, the result is an unsatisfiable @racket[solution?].  The assertions encountered while 
  evaluating @racket[assume-expr] and @racket[guarantee-expr] are removed from the global @tech["assertion store"] once 
  @racket[synthesize] returns.  These assertions @bold{may not include} @tech[#:key "quantified formula"]{quantified formulas}.
  The solver's ability to find solutions depends on the current @tech["reasoning precision"],
  as determined by the @racket[current-bitwidth] parameter. 
  @examples[#:eval rosette-eval
  (define-symbolic x c integer?)
  (assert (even? x))
  (code:line (asserts)   (code:comment "assertion pushed on the store")) 
  (define sol 
    (synthesize #:forall (list x) 
                #:guarantee (assert (odd? (+ x c)))))
  (code:line (asserts)   (code:comment "assertion store same as before")) 
  (code:line (evaluate x sol) (code:comment "x is unbound")) 
  (code:line (evaluate c sol) (code:comment "c must an odd integer"))]
}

@(rosette-eval '(clear-asserts!))

@section{Optimization}

@defform[(optimize
            maybe-minimize
            maybe-maximize
            #:guarantee guarantee-expr)
          #:grammar ([maybe-minimize (code:line) (code:line #:minimize minimize-expr)]
                     [maybe-maximize (code:line) (code:line #:maximize maximize-expr)])
          #:contracts [(minimize-expr (listof (or/c integer? real? bv?)))
                       (maximize-expr (listof (or/c integer? real? bv?)))]]{
  Searches for a binding of symbolic constants to concrete values that satisfies all assertions encountered
  before the invocation of @racket[optimize] and during the evaluation of @racket[minimize-expr],
  @racket[maximize-expr], and @racket[guarantee-expr].  
  If such a binding exists, it is returned in the form of a satisfiable @racket[solution?]; otherwise, 
  the result is an unsatisfiable solution.  Any satisfiable solution returned by @racket[optimize] is optimal with respect
  to the cost terms provided in the @racket[minimize-expr] and @racket[maximize-expr] lists.  Specifically, these
  terms take on the minimum or maximum values when evaluated with respect to a satisfiable solution.  For more details on
  solving optimization problems, see the
  @hyperlink["http://rise4fun.com/z3opt/tutorialcontent/guide"]{Z3 optimization tutorial}.

  As is the case for other solver-aided queries, the assertions encountered while 
  evaluating @racket[minimize-expr],
  @racket[maximize-expr], and @racket[guarantee-expr] are removed from the global @tech["assertion store"] once
  the query returns.  As a result, 
  @racket[optimize] has no observable effect on the @tech["assertion store"]. 
  The solver's ability to find solutions (as well as their optimality) depends on the current @tech["reasoning precision"],
  as determined by the @racket[current-bitwidth] parameter.
  
  @examples[#:eval rosette-eval
  (code:line (current-bitwidth #f) (code:comment "use infinite-precision arithmetic"))
  (define-symbolic x y integer?)
  (assert (< x 2))
  (code:line (asserts)   (code:comment "assertion added to the store")) 
  (define sol
    (optimize #:maximize (list (+ x y))
              #:guarantee (assert (< (- y x) 1))))
  (code:line (asserts)   (code:comment "assertion store same as before"))
  (code:line (evaluate x sol) (code:comment "x + y is maximal at x = 1"))
  (code:line (evaluate y sol) (code:comment "and y = 1"))]
}

@(rosette-eval '(clear-asserts!))
@(rosette-eval '(current-bitwidth 5))

@section{Debugging}

@defmodule[rosette/query/debug #:use-sources (rosette/query/debug)]

@defform[(define/debug head body ...)
         #:grammar
         ([head id (id ...)])]{
  Defines a procedure or an expression, and marks it as a candidate for debugging.  
  When a @racket[debug] query is applied to a failing execution, 
  forms that are not marked in this way are considered 
  correct.  The solver will apply the debugging algorithm only to 
  expressions and procedures marked as potentially faulty using 
  @racket[define/debug].
}

@defform[(debug [type ...+] expr)
         #:contracts
         ([type (and/c solvable? type? (not/c function?))])]{
Searches for a minimal set of @racket[define/debug] expressions of 
the given @tech["solvable type"](s) that are collectively responsible for the observed failure of @racket[expr]. 
If no expressions of the specified types are relevent to the failure, an error is thrown.  The 
returned expressions, if any, are called a minimal unsatisfiable core. The core expressions 
are relevant to the observed failure in that preventing the failure requries modifying at least one 
core expression. In particular, if all of the non-core expressions were replaced with 
fresh constants created using @racket[define-symbolic*], @racket[(solve expr)] would still fail.  It 
can only execute successfully if at least one of the core expressions is also
replaced with a fresh constant. See the @seclink["ch:essentials"]{Essentials} chapter for example uses of
the @racket[debug] form.}

@section[#:tag "sec:reasoning-precision"]{Reasoning Precision}

@defparam[current-bitwidth k (or/c #f positive-integer?)
          #:value 5]{
  A parameter that defines the current @deftech[#:key "reasoning precision"]{reasoning precision}
  for solver-aided queries over @racket[integer?] and @racket[real?] constants.
  Setting @racket[current-bitwidth] to a positive integer @racket[k] instructs Rosette to approximate
  both reals and integers with signed @racket[k]-bit words. Setting it to @racket[#f] instructs Rosette to use
  infinite precision for real and integer operations.  As a general rule, @racket[current-bitwidth] should
  be set once, before any numeric operations are evaluated. 

  Technically, when @racket[current-bitwidth] is a positive integer @racket[k],
  Rosette translates queries over reals and integers into constraints in the 
  @hyperlink["http://rise4fun.com/z3/tutorial"]{theory of bitvectors}
  (of size @racket[k]), which can be efficiently decided by SMT solvers.
  When this form of translation is used, a @racket[solve] or @racket[verify]
  query will produce a satisfiable result if and only if there is a
  solution under @racket[k]-bit semantics that is also correct under infinite-precision semantics.
  (Note that this guarantee is limited in the case of unsatisfiability---it says only
   that no @racket[k]-bit solution corresponds to an infinite-precision solution.)
  Rosette does not provide such a soundness guarantee for other queries because it is 
  computationally expensive or impossible to provide.  A @racket[synthesize] query, for example, 
  may produce a solution that is correct under @racket[k]-bit semantics, but incorrect under
  infinite-precision semantics. 

  When  @racket[current-bitwidth] is @racket[#f], Rosette translates queries over
  reals and integers into constraints in the
  @hyperlink["http://rise4fun.com/z3/tutorial"]{theories of reals and integers}. 
  These theories are effectively decidable only for linear constraints,
  so most applications will perform better when @racket[current-bitwidth] is
  set to a positive integer.

  The @racket[current-bitwidth] parameter must be set to @racket[#f] when
  executing queries over assertions that contain @tech[#:key "quantified formula"]{quantified formulas}.
  Otherwise, such a query will throw an exception.

  @examples[
 #:eval rosette-eval
 (define-symbolic x y real?)
 (current-bitwidth 5)  
 (code:line (solve (assert (= x 3.5))) (code:comment "there is no solution under"))
 (code:line (solve (assert (= x 64)))  (code:comment "5-bit signed integer semantics"))
 (code:line (solve (assert (forall (list x) (= x (+ x y)))))  (code:comment "and quantifiers are not supported"))
 (current-bitwidth #f)
 (code:line (solve (assert (= x 3.5))) (code:comment "but there is a solution under"))
 (code:line (solve (assert (= x 64)))  (code:comment "infinite-precision semantics"))
 (code:line (solve (assert (forall (list x) (= x (+ x y)))))  (code:comment "and quantifiers work"))]
}

 


@(kill-evaluator rosette-eval)
