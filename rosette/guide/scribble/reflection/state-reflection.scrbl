#lang scribble/manual

@(require (for-label 
           rosette/query/query rosette/base/form/define
           (only-in rosette/base/base
                    assert assume
                    vc vc? vc-assumes vc-asserts
                    clear-vc! with-vc vc-true vc-true?
                    result? result-value result-state
                    ans ans? halt halt? clear-terms! term-cache weak-term-cache)
           racket)
          scribble/core scribble/html-properties scribble/example racket/sandbox
          racket/runtime-path 
          "../util/lifted.rkt")


@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "state-log")))

@title[#:tag "sec:state-reflection"]{Reflecting on Symbolic State}


Like standard program execution, Rosette's @tech{symbolic
 evaluation} can be viewed as a function that operates on
@deftech[#:key "program state"]{program states}. The state
of a Rosette program includes its Racket state (memory,
environment, and so on) and its symbolic state. The key
parts of the symbolic state are the current @deftech{verification condition}
(VC) and the current @deftech{symbolic heap}.
The heap consists of all the @tech[#:key "symbolic term"]{symbolic terms}
created so far. The verification condition tracks all the
@racketlink[assume]{assumptions} and @racketlink[assert]{assertions}
issued on @emph{any} path between the starting program state
and the current state. Unlike concrete execution, which
evaluates just one path through a program, Rosette evaluates
all possible paths when the decision on which path to take
depends on a symbolic value. The symbolic heap and the
verification condition reflect the current state of this
all-path evaluation.

@(rosette-eval '(require (only-in racket hash-values)))
@(rosette-eval '(require (only-in rosette/guide/scribble/util/lifted opaque)))
@examples[#:eval rosette-eval #:label #f #:no-prompt
(define-symbolic a b c d boolean?)
(code:line (hash-values (term-cache)) (code:comment "Symbolic heap."))
(assume a)
(code:line (vc) (code:comment "Verification condition."))
(if b
    (begin
      (printf "Then branch:\n ~a\n" (vc))
      (assert c)
      (printf " ~a\n" (vc))
      1)
    (begin
      (printf "Else branch:\n ~a\n" (vc))
      (assert d)
      (printf " ~a\n" (vc))
      2))
(vc)
(eval:alts
 (hash-values (term-cache))
 (append (take (hash-values (term-cache)) 5) (list (opaque "..."))))]

This section describes the built-in facilities for accessing
and modifying various aspects of the symbolic state from
within a Rosette program. These facilites are useful for
low-level debugging and optimizations of Rosette-based
tools. But they also make it possible to 
violate the state invariants maintained by Rosette, so it is
best to use them sparingly.

@declare-exporting[rosette/base/base
                   #:use-sources
                   (rosette/base/base)]

@(rosette-eval '(clear-vc!))
@section[#:tag "sec:vc"]{Verification Conditions}

@deftogether[(@defproc[(vc? [v any/c]) boolean?]
              @defproc[(vc-assumes [v vc?]) boolean?]
              @defproc[(vc-asserts [v vc?]) boolean?])]{
                                                   
 A verification condition (VC) is a structure of type
 @racket[vc?]. It consists of two (concrete or symbolic)
 boolean values that reflect the assumptions and assertions
 issued during symbolic evaluation. Programs can access these
 values using @racket[vc-assumes] and @racket[vc-asserts]. }

@deftogether[(@defthing[vc-true vc?]
              @defproc[(vc-true? [v any/c]) boolean?])]{
                                                        
 The true verification condition, @racket[vc-true], is a
 value of type @racket[vc?] that consists of a true 
 assumption and assertion. The true verification condition
 is recognized by @racket[vc-true?].
 
 @examples[#:eval rosette-eval
 vc-true
 (vc? vc-true)
 (vc-assumes vc-true)
 (vc-asserts vc-true)
 (vc-true? vc-true)
 (vc-true? 1)]        
 
}

                                                   
@defproc[(vc) vc?]{
                                                   
 The current @tech{verification condition}, @racket[(vc)],
 is a value of type @racket[vc?]. At the start of evaluation,
 @racket[(vc)] has the value @racket[vc-true]. As the
 evaluation progresses along all paths, the current
 @racket[(vc)] accumulates all the assertions and assumptions
 issued on these paths.

 The current @racket[(vc)] state can be cleared using
 @racket[clear-vc!]. This resets @racket[(vc)] to its
 starting value, @racket[vc-true]. 


@examples[#:eval rosette-eval
(vc)
(define-symbolic a b boolean?)
(vc)
(assume a)
(vc)
(assert b)
(vc)
(clear-vc!)
(vc)]
}

@defproc[(clear-vc!) void?]{
Clears the current verification condition by setting it to @racket[vc-true].
See @racket[vc] for details.               
}

@defform*[((with-vc expr)
           (with-vc vc-expr expr))
          #:contracts ([vc-expr vc?])]{

 Evaluates @racket[expr] with @racket[(vc)] set to
 @racket[vc-expr], returns the result, and restores
 @racket[(vc)] to its old value. If @racket[vc-expr] is not
 given, it defaults to @racket[(vc)], so
 @racket[(with-vc expr)] is equivalent to
 @racket[(with-vc (vc) expr)].

 The result of a @racket[with-vc] expression is a value of
 type @racket[result?]. If @racket[expr] terminates normally,
 then the result is @racket[ans?], and its
 @racket[result-value] contains the value computed by
 @racket[expr]. If @racket[expr] fails, the result is
 @racket[halt?], and its @racket[result-value] contains an
 @racket[exn:fail?] exception that represents the cause of
 the abnormal termination. In either case,
 @racket[result-state] holds the final verification condition
 generated during the evaluation of @racket[expr], starting
 with @racket[vc-expr], or @racket[(vc)], as the initial
 verification condition.

 In the context of symbolic evaluation, @racket[expr]
 terminates normally if it has at least one path of execution
 that terminates normally. A path terminates normally if it
 is free of any assumption or assertion violations---i.e., any 
 @racket[(assume #, @var{e})] or @racket[(assert #, @var{e})]
 expressions where @var{e} evaluates to @racket[#f]. Failures
 due to exceptions are treated as assertion violations.

 @examples[#:eval rosette-eval
 (define-symbolic a b boolean?)
 (assume a)
 (vc)
 (code:comment "Normal termination, starting with (vc).")
 (code:comment "The error along the path where b is true")
 (code:comment "is treated as an assertion violation.")
 (with-vc
   (begin
     (when b      
       (error 'b "No b allowed!"))
     1))
 (code:line (vc) (code:comment "(vc) is unchanged."))
 (code:comment "Abnormal termination, starting with vc-true.")
 (with-vc vc-true
   (if b
       (assume #f)
       (assert #f)))
 (code:line (vc) (code:comment "(vc) is unchanged."))]
}

@deftogether[(@defproc[(result? [v any/c]) boolean?]
              @defproc[(result-value [v result?]) any/c]
              @defproc[(result-state [v result?]) any/c] 
              @defproc[(ans?  [v any/c]) boolean?]
              @defproc[(halt? [v any/c]) boolean?])]{

 A @racket[result?] value represents the result of symbolic
 evaluation, which includes an output value,
 @racket[(result-value v)], and a part of the output state,
 @racket[(result-state v)]. Every result is either
 @racket[ans?], if the evaluation terminated normally, or
 @racket[halt?] otherwise.

 See also @racket[with-vc].
 
}

@section[#:tag "sec:heap"]{Symbolic Heap}


@(rosette-eval '(clear-terms!))
@(rosette-eval '(clear-vc!))
@defparam[term-cache h hash?]{
                              
 A parameter that holds the cache of all
 @seclink["sec:symbolic-terms"]{symbolic terms} generated
 during symbolic evaluation. These terms form the current
 @tech{symbolic heap}. Rosette uses the cache to ensure that
 no syntactically identical terms are created.  

 Programs @emph{should not modify} the term cache, since
 doing so can lead to incorrect behavior.

 The cache is not garbage collected, which means that long
 running applications may experience memory pressure. There
 are two ways to solve this issue, depending on the
 application's behavior.

 If the application is performing many independent queries
 that do @emph{not} share @emph{any} symbolic constants, then
 the application may safely clear the term cache between
 query invocations using @racket[clear-terms!]. This is the
 most common scenario.
  
 If the application is solving a series of related queries,
 using @racket[clear-terms!] can lead to incorrect behavior.
 In this case, the safe solution is to use a
 garbage-collected cache by setting @racket[term-cache] to
 @racket[(weak-term-cache)]. This setting adds
 runtime overhead, so it is best saved for when the default
 cache consumes too much memory.
 

@examples[#:eval rosette-eval
(hash-values (term-cache))
(define (get-a)
  (define-symbolic a integer?)
  a)
(define a0 (get-a))
(+ a0 3)
(hash-values (term-cache))
(code:comment "In the following scenario, using clear-terms! leads to")
(code:comment "incorrect behavior: (query-a a0) should always return")
(code:comment "unsat? according to the semantics of define-symbolic.")
(code:comment "But if the cache is cleared, it returns sat? because a0")
(code:comment "is bound to a constant that no longer exists in the cache.")
(define (query-a a)
  (verify
   (assert (= a (get-a)))))
(query-a a0)
(hash-values (term-cache))
(clear-terms!)
(hash-values (term-cache))
(code:line (query-a a0) (code:comment "Wrong result."))
(hash-values (term-cache))]
}                              


@defproc[(clear-terms! [terms (or/c #f (listof term?)) #f]) void?]{
                                                                   
 Clears the entire term-cache if invoked with @racket[#f]
 (default), or it evicts all of the given @racket[terms] as
 well as any expressions that transitively contain them.
 Clearing the term cache is @emph{not} safe in general; see
 @racket[term-cache] for details and examples.
}

@(rosette-eval '(clear-terms!))
@(rosette-eval '(require (only-in racket collect-garbage hash-count)))
@defproc[(weak-term-cache) hash?]{

 Returns a term cache that cooperates with Racket's garbage
 collector. The resulting cache is initialized with the
 reachable bindings from @racket[(term-cache)]. Setting the
 @racket[term-cache] parameter to a cache returned by
 @racket[(weak-term-cache)] ensures that unused terms are
 garbage-collected throughout symbolic evaluation.

 @examples[#:eval rosette-eval
 (code:comment "Creates n unreachable terms of the form (+ a (+ a ...)).")
 (define (unused-terms! n)
  (define-symbolic* a integer?)
  (let loop ([n n])
    (if (<= n 1)
        a
        (+ a (loop (- n 1)))))
  (void))
 (code:line (hash-count (term-cache))      (code:comment "Empty term cache."))
 (time (unused-terms! 50000))
 (hash-count (term-cache))
 (collect-garbage)
 (code:line (hash-count (term-cache))      (code:comment "GC has no effect on the default cache."))
 (clear-terms!)
 (collect-garbage)
 (code:line (term-cache (weak-term-cache)) (code:comment "Use a weak-term-cache."))
 (hash-count (term-cache))
 (time (unused-terms! 50000))
 (eval:alts (hash-count (term-cache)) 50000)
 (collect-garbage)
 (code:line (hash-count (term-cache))      (code:comment "A weak-term-cache cooperates with GC."))
]
                                  
}


@(kill-evaluator rosette-eval)


