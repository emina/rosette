#lang scribble/manual

@(require (for-label 
           rosette/query/query rosette/base/form/define
           (only-in rosette/base/base
                    assert assume term?
                    vc vc? vc-assumes vc-asserts
                    clear-vc! with-vc vc-true vc-true?
                    result? result-value result-state
                    normal normal? failed failed?
                    terms with-terms clear-terms! gc-terms!)
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

@(rosette-eval '(require (only-in rosette/guide/scribble/util/lifted opaque)))
@examples[#:eval rosette-eval #:label #f #:no-prompt
(define-symbolic a b c d boolean?)
(code:line (terms) (code:comment "Symbolic heap."))
(assume a)
(code:line (vc)    (code:comment "Verification condition."))
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
 (terms)
 (append (take (terms) 5) (list (opaque "..."))))]

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
 then the result is @racket[normal?], and its
 @racket[result-value] contains the value computed by
 @racket[expr]. If @racket[expr] fails, the result is
 @racket[failed?], and its @racket[result-value] contains an
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
              @defproc[(normal?  [v any/c]) boolean?]
              @defproc[(failed? [v any/c]) boolean?])]{

 A @racket[result?] value represents the result of symbolic
 evaluation, which includes an output value,
 @racket[(result-value v)], and a part of the output state,
 @racket[(result-state v)]. Every result is either
 @racket[normal?], if the evaluation terminated normally, or
 @racket[failed?] otherwise.

 See also @racket[with-vc].
 
}

@section[#:tag "sec:heap"]{Symbolic Heap}


@(rosette-eval '(clear-terms!))
@(rosette-eval '(clear-vc!))


@defproc[(terms) (listof term?)]{
                              
 Returns a list of all @seclink["sec:symbolic-terms"]{symbolic terms}
 generated during symbolic evaluation. These
 terms form the contents of the current @tech{symbolic heap}.
 Rosette uses the symbolic heap to ensure that no
 syntactically identical terms are created. 

 The symbolic heap is not garbage collected, which means
 that long running applications may experience memory
 pressure. There are two ways to solve this issue, depending
 on the application's behavior.

 If the application is performing many independent queries
 that do @emph{not} share @emph{any} symbolic constants, then
 the application may safely clear the heap between
 query invocations using @racket[clear-terms!]. This is the
 most common scenario.
  
 If the application is solving a series of related queries,
 using @racket[clear-terms!] can lead to incorrect behavior.
 In this scenario, the safe solution is to use a
 garbage-collected heap by invoking @racket[(gc-terms!)].
 This has the effect of changing the internal representation
 of the heap to use a more expensive data structure that
 cooperates with Racket's garbage collector. Because of the
 added runtime overhead, this setting is best saved for when
 the default heap consumes too much memory.

@examples[#:eval rosette-eval
(terms)
(define (get-a)
  (define-symbolic a integer?)
  a)
(define a0 (get-a))
(+ a0 3)
(terms)
(code:comment "In the following scenario, using clear-terms! leads to")
(code:comment "incorrect behavior: (query-a a0) should always return")
(code:comment "unsat? according to the semantics of define-symbolic.")
(code:comment "But if the heap is cleared, it returns sat? because a0")
(code:comment "is bound to a constant that no longer exists in the heap.")
(define (query-a a)
  (verify
   (assert (= a (get-a)))))
(query-a a0)
(terms)
(clear-terms!)
(terms)
(code:line (query-a a0) (code:comment "Wrong result."))
(terms)]
}                              


@defproc[(clear-terms! [ts (or/c #f (listof term?)) #f]) void?]{

 Clears the symbolic heap of all terms if @racket[ts] is
 false; otherwise, clears all the terms in @racket[ts] and
 any expressions that transitively contain them. Clearing the
 heap is @emph{not} safe in general; see @racket[terms] for
 details and examples. }

@(rosette-eval '(clear-terms!))
@(rosette-eval '(require (only-in racket collect-garbage)))
@defproc[(gc-terms!) void?]{

 Changes the internal representation of the symbolic heap to
 a data structure that cooperates with Racket's garbage
 collector. The resulting heap is initialized with the
 reachable terms from the current symbolic heap, as given by
 @racket[(terms)]. This setting ensures that unused terms are
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
 (code:line (length (terms))   (code:comment "Empty heap."))
 (time (unused-terms! 50000))
 (length (terms))
 (collect-garbage)
 (code:line (length (terms))   (code:comment "GC has no effect on the default heap."))
 (clear-terms!)
 (collect-garbage)
 (code:line (gc-terms!)        (code:comment "Use gc-terms! to change the representation."))
 (length (terms))
 (time (unused-terms! 50000))
 (eval:alts (length (terms)) 50000)
 (collect-garbage)
 (code:line (length (terms))   (code:comment "The heap now cooperates with GC."))
]
                                  
}


@defform*[((with-terms expr)
           (with-terms terms-expr expr))
          #:contracts ([terms-expr (listof term?)])]{

 Evaluates @racket[expr] with @racket[(terms)] set to
 @racket[terms-expr], returns the result, and restores
 @racket[(terms)] to the value it held before the evaluation
 of @racket[expr]. If @racket[terms-expr] is not given, it
 defaults to @racket[(terms)], so @racket[(with-terms expr)]
 is equivalent to @racket[(with-terms (terms) expr)].

 The result of a @racket[with-terms] expression is the output 
 of @racket[expr].
 
 Note that none of the terms created during the evaluation
 of @racket[expr] are preserved in the heap, and having those
 terms escape the dynamic extent of @racket[expr] is usually
 a sign of a programming error. It can lead to incorrect
 behavior, similarly to using @racket[clear-terms!].

 @examples[#:eval rosette-eval
 (define-symbolic a b integer?)
 (terms)
 (with-terms
   (begin0
     (verify (assert (= (+ a b) 0)))
     (println (terms))))
 (terms)
 (with-terms (list) ; Empty heap
   (begin
     (println (terms))
     (define-symbolic c integer?)
     (println (terms))))
 (terms)]
}

@(kill-evaluator rosette-eval)


