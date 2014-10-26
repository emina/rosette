#lang scribble/manual

@(require (for-label  
           rosette/base/define rosette/query/tools rosette/query/eval rosette/solver/solution
           rosette/base/term (only-in rosette/query/debug define/debug debug)
           (only-in rosette/base/safe assert) 
           (only-in rosette/base/assert asserts)
           (only-in rosette/base/enum enum?))
          (for-label racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")

@(define rosette-eval (rosette-evaluator))

@title[#:tag "ch:syntactic-forms:rosette"]{Solver-Aided Forms}

The @seclink["ch:essentials"]{Essentials} chapter introduced the key concepts of solver-aided programming.  This section defines the corresponding syntactic constructs more precisely.

@declare-exporting[rosette/base/define 
                   rosette/query/tools 
                   rosette/base/safe
                   #:use-sources 
                   (rosette/base/define 
                   rosette/query/tools 
                   rosette/base/safe)]

@section[#:tag "sec:symbolic-constants-and-assertions"]{Symbolic Constants and Assertions}

@defform[(define-symbolic id ...+ type)
         #:contracts
         [(type (or/c boolean? number? enum?))]]{
  Binds each provided identifier to a distinct @tech["symbolic constant"] of the given 
  primitive or enumeration type.  The identifiers are bound to the same constants every time the form is 
  evaluated.  
  @examples[#:eval rosette-eval
  (define (always-same)
    (define-symbolic x number?)
    x)
  (always-same)
  (always-same) 
  (eq? (always-same) (always-same))]
}
@defform[(define-symbolic* id ...+ type)
         #:contracts
         [(type (or/c boolean? number? enum?))]]{
  Creates a stream of distinct @tech["symbolic constant"] of the given 
  type for each identifier, binding the identifier to the 
  next element from its stream every time the form is evaluated.  
  @examples[#:eval rosette-eval
  (define (always-different)
    (define-symbolic* x number?)
    x)
  (always-different) 
  (always-different) 
  (eq? (always-different) (always-different))]
}


@defform[(assert expr maybe-message)
         #:grammar
         [(maybe-message (code:line) expr)]]{
  Checks that @racket[expr] is a non-false value, and if it is, throws an error using the 
  optional failure message. If @racket[expr] evaluates to a symbolic boolean value,  
  that value is pushed onto the stack of assertions that will eventually be used to formulate 
  a query to the underlying solver.
  @examples[#:eval rosette-eval
  (code:line (assert #t) (code:comment "no effect"))
  (code:line (assert 1)  (code:comment "no effect"))
  (code:line (asserts)   (code:comment "empty assertion stack"))  
  (define-symbolic x boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x pushed onto the assertion stack"))  
  (assert #f "bad value")]
}


@section{Angelic Execution, Verification, and Synthesis}

@(rosette-eval '(clear-asserts))


@defform[(solve expr)]{
  Searches for a binding of symbolic constants to concrete values that satisfies all assertions encountered
  before the invocation of @racket[solve] and during the evaluation of @racket[expr]. 
  If such a binding exists, it is returned in the form of a satisfiable @racket[solution?]; otherwise, 
  an error is thrown.  The assertions encountered while 
  evaluating @racket[expr] are removed from the global assertion stack once @racket[solve] returns.  As a result, 
  @racket[solve] has no observable effect on the assertion stack. We refer to the  
  @racket[solve] query as @deftech{angelic execution} because it causes the solver to behave as an angelic oracle--- 
  it supplies "good" bindings for symbolic constants that cause the execution to terminate successfully.
  @examples[#:eval rosette-eval
  (define-symbolic x y boolean?)
  (assert x)
  (code:line (asserts)   (code:comment "x pushed onto the assertion stack"))  
  (define sol (solve (assert y)))
  (code:line (asserts)   (code:comment "assertion stack same as before")) 
  (code:line (evaluate x sol) (code:comment "x must be true"))
  (code:line (evaluate y sol) (code:comment "y must be true"))
  (solve (assert (not x)))]
}

@;@(rosette-eval '(clear-asserts))
@;@defform[(solve/evaluate expr)]{
@;  Invokes @racket[solve] on @racket[expr] to obtain a satisfying solution, and 
@;  returns the result of evaluating @racket[expr]
@;  with respect to that solution.  Throws an error if no satisfying solution is found.
@;  @examples[#:eval rosette-eval
@;  (define-symbolic x y boolean?)
@;  (assert x)
@;  (solve/evaluate (begin (assert y) (cons x y)))]
@;}

@(kill-evaluator rosette-eval)
@(set! rosette-eval (rosette-evaluator))
@defform*[((verify guarantee-expr)
           (verify #:assume assume-expr #:guarantee guarantee-expr))]{
  Searches for a binding of symbolic constants to concrete values that violates at least one of the 
  assertions encountered during the evaluation of @racket[guarantee-expr], but that satisfies all 
  assertions encountered before the invocation of @racket[verify] and during the evaluation of 
  @racket[assume-expr]. If such a binding exists, it is returned in the form of a
  satisfiable @racket[solution?]; otherwise, an error is thrown.  The assertions encountered while 
  evaluating @racket[assume-expr] and @racket[guarantee-expr] are removed from the global assertion stack once 
  @racket[verify] returns.  
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

@(rosette-eval '(clear-asserts))
@defform[(synthesize #:forall input-expr 
                      maybe-init 
                      maybe-assume 
                      #:guarantee guarantee-expr)
         #:grammar 
         ([maybe-init (code:line) (code:line #:init init-expr)]
          [maybe-assume (code:line) (code:line #:assume assume-expr)])
         #:contracts 
         ([input-expr (listof constant?)]
          [init-expr (or/c (and/c sat? solution?) (listof (and/c sat? solution?)))])]{
  Searches for a binding of symbolic constants 
  to concrete values that has the following properties: 
  @itemlist[#:style 'ordered
  @item{it does not map constants in the @racket[input-expr] list; and,} 
  @item{it satisfies all assertions encountered during the evaluation of 
  @racket[guarantee-expr], for every binding of @racket[input-expr] constants to values that satisfies 
  the assertions encountered before the invocation of @racket[synthesize] and during the evaluation of 
  @racket[assume-expr].}] 
  If no such binding exists, an error is thrown.  The assertions encountered while 
  evaluating @racket[assume-expr] and @racket[guarantee-expr] are removed from the global assertion stack once 
  @racket[synthesize] returns.  The optional @racket[init-expr], if given, must evaluate to bindings for constants 
  in @racket[input-expr] that satisfy all assertions  encountered before the invocation of @racket[synthesize] 
  and during the evaluation of @racket[assume-expr]. Providing these optional bindings may speed up the query.
  @examples[#:eval rosette-eval
  (define-symbolic x c number?)
  (assert (even? x))
  (code:line (asserts)   (code:comment "assertion pushed on the stack")) 
  (define sol 
    (synthesize #:forall (list x) 
                #:guarantee (assert (= (/ x 2) (>> x c)))))
  (code:line (asserts)   (code:comment "assertion stack same as before")) 
  (code:line (evaluate x sol) (code:comment "the value of x is unknown")) 
  (code:line (evaluate c sol) (code:comment "c must be 1"))]
}

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
         ([type (or/c boolean? number? enum?)])]{
Searches for a minimal set of @racket[define/debug] expressions of 
the given type(s) that are collectively responsible for the observed failure of @racket[expr]. 
If no expressions of the specified types are relevent to the failure, an error is thrown.  The 
returned expressions, if any, are called a minimal unsatisfiable core. The core expressions 
are relevant to the observed failure in that it cannot be prevented without modifying at least one 
core expression. In particular, if all of the non-core expressions were replaced with 
fresh constants created using @racket[define-symbolic*], @racket[(solve expr)] would still fail.  It 
can only execute successfully if at least one of the core expressions is also replaced with a fresh constant.}

@(kill-evaluator rosette-eval)