#lang scribble/manual

@(require (for-label  
           rosette/base/form/define rosette/query/form rosette/query/eval rosette/solver/solution
           rosette/base/core/term (only-in rosette/query/debug define/debug debug)
           (only-in rosette/query/query current-bitwidth)
           (only-in rosette/base/core/safe assert) 
           (only-in rosette/base/core/bool asserts clear-asserts!))
          (for-label racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")

@(define rosette-eval (rosette-evaluator))

@title[#:tag "ch:syntactic-forms:rosette"]{Solver-Aided Forms}

The @seclink["ch:essentials"]{Essentials} chapter introduced the key concepts of solver-aided programming.  This section defines the corresponding syntactic constructs more precisely.

@declare-exporting[rosette/base/form/define 
                   rosette/query/form 
                   rosette/base/core/safe
                   rosette/base/core/bool
                   #:use-sources 
                   (rosette/base/form/define 
                   rosette/query/form
                   rosette/base/core/safe
                   rosette/base/core/bool)]

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
         [(maybe-message (code:line) expr)]
         #:contracts
         [(expr (or/c string? procedure?))]]{
  Provides a mechanism for communicating desired
  program properties to the underlying solver.  Rosette keeps track of all
  assertions evaluated during an execution via an @deftech[#:key "assertion stack"]{assertion stack}. 
  If @racket[expr] evaluates to @racket[#f], an error is thrown using the 
  optional failure message, and @racket[#f] is pushed onto the assertion stack.  The error message
  can be either a string or a no-argument procedure that throws an error when called.
  If @racket[expr] evaluates to a symbolic boolean value, that value is pushed onto the assertion stack.
  If @racket[expr] evaluates to any other value, @racket[assert] has no effect.  The contents
  of the assertion stack can be examined using the @racket[asserts] procedure, and it can be
  cleared using the @racket[clear-asserts!] procedure. 
  @examples[#:eval rosette-eval
  (code:line (assert #t) (code:comment "no effect"))
  (code:line (assert 1)  (code:comment "no effect"))
  (code:line (asserts)   (code:comment "retrieve the assertion stack"))  
  (define-symbolic x boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x pushed onto the assertion stack"))  
  (assert #f "bad value")
  (asserts) 
  (code:line (clear-asserts!)   (code:comment "clear the assertion stack"))
  (asserts)]
}

@defproc[(asserts) (listof boolean?)]{
 Returns the contents of the @tech["assertion stack"] as a list of symbolic
 boolean values (or @racket[#f]) that have been asserted so far. See the @racket[assert] form for examples.                                        
}

@defproc[(clear-asserts!) void?]{
 Clears the @tech["assertion stack"] from all symbolic
 boolean values (or @racket[#f]) that have been asserted so far. See the @racket[assert] form for examples.                         
}

@section{Angelic Execution}

@defform[(solve expr)]{
  Searches for a binding of symbolic constants to concrete values that satisfies all assertions encountered
  before the invocation of @racket[solve] and during the evaluation of @racket[expr]. 
  If such a binding exists, it is returned in the form of a satisfiable @racket[solution?]; otherwise, 
  the result is an unsatisfiable solution.  The assertions encountered while 
  evaluating @racket[expr] are removed from the global @tech["assertion stack"] once @racket[solve] returns.  As a result, 
  @racket[solve] has no observable effect on the @tech["assertion stack"]. 
  The solver's ability to find solutions depends on the current @tech["reasoning precision"], as
  determined by the @racket[current-bitwidth] parameter.
  @examples[#:eval rosette-eval
  (define-symbolic x y boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x pushed onto the assertion stack"))  
  (define sol (solve (assert y)))
  (code:line (asserts)   (code:comment "assertion stack same as before")) 
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
  evaluating @racket[assume-expr] and @racket[guarantee-expr] are removed from the global @tech["assertion stack"] once 
  @racket[verify] returns.   
  The solver's ability to find solutions depends on the current @tech["reasoning precision"], as
  determined by the @racket[current-bitwidth] parameter.
  @examples[#:eval rosette-eval
  (define-symbolic x y boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x pushed onto the assertion stack")) 
  (define sol (verify (assert y)))
  (code:line (asserts)   (code:comment "assertion stack same as before")) 
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
          #:contracts [(input-expr (listof? constant?))]]{
  Searches for a binding of symbolic constants 
  to concrete values that has the following properties: 
  @itemlist[#:style 'ordered
  @item{it does not map constants in the @racket[input-expr] list; and,} 
  @item{it satisfies all assertions encountered during the evaluation of 
  @racket[guarantee-expr], for every binding of @racket[input-expr] constants to values that satisfies 
  the assertions encountered before the invocation of @racket[synthesize] and during the evaluation of 
  @racket[assume-expr].}] 
  If no such binding exists, the result is an unsatisfiable @racket[solution?].  The assertions encountered while 
  evaluating @racket[assume-expr] and @racket[guarantee-expr] are removed from the global @tech["assertion stack"] once 
  @racket[synthesize] returns.  The solver's ability to find solutions depends on the current @tech["reasoning precision"],
  as determined by the @racket[current-bitwidth] parameter.
  @examples[#:eval rosette-eval
  (define-symbolic x c integer?)
  (assert (even? x))
  (code:line (asserts)   (code:comment "assertion pushed on the stack")) 
  (define sol 
    (synthesize #:forall (list x) 
                #:guarantee (assert (odd? (+ x c)))))
  (code:line (asserts)   (code:comment "assertion stack same as before")) 
  (code:line (evaluate x sol) (code:comment "x is unbound")) 
  (code:line (evaluate c sol) (code:comment "c must an odd integer"))]
}

@(rosette-eval '(clear-asserts!))

@section{Optimization}

@section{Debugging}

@(kill-evaluator rosette-eval)