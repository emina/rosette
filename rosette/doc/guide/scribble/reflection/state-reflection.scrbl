#lang scribble/manual

@(require (for-label 
           rosette/solver/solver rosette/solver/solution rosette/query/state
           rosette/solver/kodkod/kodkod 
           rosette/base/define rosette/query/tools rosette/query/eval rosette/solver/solution
           rosette/base/term rosette/base/type rosette/base/primitive rosette/base/enum rosette/base/union
           rosette/base/forall rosette/lib/reflect/lift 
           (only-in rosette/base/assert pc asserts clear-asserts with-asserts with-asserts-only)
           (only-in rosette/base/safe assert) 
           racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")
@(require (only-in "../refs.scrbl" ~cite rosette:pldi14))

@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:state-reflection"]{Reflecting on Symbolic State}

Like standard program execution, Rosette's symbolic evaluation @~cite[rosette:pldi14] can be understood as a sequence of transitions from one @deftech{program state} to the next. In addition to the memory and register values, the state of a Rosette program also includes the current @deftech{path condition} and the current @deftech{assertion store}. The path condition is a boolean value encoding the branch decisions taken to reach the present state, and the assertion store is the set of boolean values (i.e., constraints) that have been asserted so far. This section describes the built-in facilities for accessing and modifying various aspects of the symbolic state from within a Rosette program. 

@declare-exporting[rosette/base/assert #:use-sources (rosette/base/assert)]

@defproc[(pc) boolean?]{
Returns the current path condition. 
@examples[#:eval rosette-eval
(define-symbolic a b boolean?)
(if a
    (if b
        #f
        (pc))
    #f)]
}

@defproc[(asserts) (listof boolean?)]{
Returns the current assertion store. 
@examples[#:eval rosette-eval
(define-symbolic a b boolean?)
(assert a)
(asserts)
(assert b)
(asserts)]
}
@(rosette-eval '(clear-asserts))


@defproc[(clear-asserts) void?]{
Empties the current assertion store. 
@examples[#:eval rosette-eval
(define-symbolic a b boolean?)
(assert a)
(assert b)
(asserts)
(clear-asserts)
(asserts)]
}

@(rosette-eval '(clear-asserts))
@defform[(with-asserts expr)]{
Returns two values: the result of evaluating @racket[expr] and the assertions 
generated during the evaluation of @racket[expr]. These  
assertions will not appear in the assertion store after 
@racket[with-asserts] returns.

@examples[#:eval rosette-eval
(define-symbolic a b boolean?)  
(define-values (result asserted) 
  (with-asserts 
   (begin (assert a) 
          (assert b) 
          4)))
(printf "result = ~a\n" result)
(printf "asserted = ~a\n" asserted)
(asserts)
]
}

@(kill-evaluator rosette-eval)


