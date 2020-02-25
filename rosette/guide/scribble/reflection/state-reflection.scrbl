#lang scribble/manual

@(require (for-label 
           rosette/query/query rosette/base/form/define
           rosette/base/core/term 
           (only-in rosette/base/core/bool pc asserts clear-asserts! with-asserts with-asserts-only)
           (only-in rosette/base/core/safe assert) 
           racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")
@(require (only-in "../refs.scrbl" ~cite rosette:pldi14))

@(define rosette-eval (rosette-evaluator))

@title[#:tag "sec:state-reflection"]{Reflecting on Symbolic State}

Like standard program execution, Rosette's symbolic evaluation @~cite[rosette:pldi14] can be
understood as a sequence of transitions from one @deftech{program state} to the next. In
addition to the memory and register values, the state of a Rosette program also includes
the current @deftech{path condition} and the current @deftech{assertion store}. The path
condition is a boolean value encoding the branch decisions taken to reach the present state,
and the assertion store is the set of boolean values (i.e., constraints) that have been asserted
so far. This section describes the built-in facilities for accessing and modifying various
aspects of the symbolic state from within a Rosette program.  In general, these facilities should
only be used by expert Rosette programmers to implement low-level solver-aided tools.  

@declare-exporting[rosette/base/base #:use-sources (rosette/base/core/bool rosette/base/core/term)]

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
Returns the contents of the @tech["assertion store"] as a list of symbolic
boolean values (or @racket[#f]) that have been asserted so far. 
@examples[#:eval rosette-eval
(define-symbolic a b boolean?)
(assert a)
(asserts)
(assert b)
(asserts)]
}
@(rosette-eval '(clear-asserts!))

@defproc[(clear-asserts!) void?]{
 Clears the @tech["assertion store"] from all symbolic
 boolean values (or @racket[#f]) that have been asserted so far.
 Rosette programs @emph{should not clear} the assertion
 store unless they are implementing low-level tools.
 @examples[#:eval rosette-eval
 (define-symbolic a b boolean?)
 (assert a)
 (assert b)
 (asserts)
 (clear-asserts!)
 (asserts)]
}

@(rosette-eval '(clear-asserts!))
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

@defform[(with-asserts-only expr)]{
Like @racket[with-asserts], but returns only the assertions generated
during the evaluation of @racket[expr].
@examples[#:eval rosette-eval
(define-symbolic a b boolean?)  
(with-asserts-only 
   (begin (assert a) 
          (assert b) 
          4))
]
}

@(rosette-eval '(clear-terms!))
@defparam[term-cache h hash?]{
A parameter that holds the cache of all @seclink["sec:symbolic-terms"]{symbolic terms}
constructed during symbolic evaluation. This cache stores terms for the purposes of
partial cannonicalization.  In particular, Rosette uses the term cache to ensure that no
syntactically identical terms are created. Rosette programs  
@emph{should not modify} the term cache unless they are implementing low-level tools.
@examples[#:eval rosette-eval
(pretty-print (term-cache))         
(define-symbolic a b c d integer?)
(pretty-print (term-cache))
(* d (- (+ a b) c))
(pretty-print (term-cache))]
}                              

@(rosette-eval '(clear-terms!))
@defproc[(clear-terms! [terms (or/c #f (listof term?)) #f]) void?]{
Clears the entire term-cache if invoked with @racket[#f] (default), or 
it evicts all of the given @racket[terms] as well as any expressions that transitively
contain them. Rosette programs @emph{should not clear} the term cache unless they are
implementing low-level tools.
@examples[#:eval rosette-eval
(term-cache)                 
(define-symbolic a b c d integer?)
(pretty-print (term-cache))
(* d (- (+ a b) c))
(pretty-print (term-cache))
(clear-terms! (list c d))
(pretty-print (term-cache))
(clear-terms!)
(pretty-print (term-cache))]
}                                                                   

@(kill-evaluator rosette-eval)


