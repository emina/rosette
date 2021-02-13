#lang scribble/manual

@(require (for-label  
           rosette/base/form/define rosette/query/query rosette/solver/solution
           (only-in rosette/solver/solver solver?)
           rosette/base/core/term 
           (only-in rosette/query/finitize current-bitwidth)
           (only-in rosette/base/base
                    assert assume vc vc-asserts vc-assumes clear-vc!
                    bv? forall)
           (only-in rosette/base/core/function function? ~>)
           (only-in rosette/base/core/reflect symbolics))
          (for-label racket)
          scribble/core scribble/html-properties scribble/examples racket/sandbox  racket/runtime-path 
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "rosette-forms-log")))

@title[#:tag "ch:syntactic-forms:rosette"]{Solver-Aided Forms}

The @seclink["ch:essentials"]{Essentials} chapter introduced
the key concepts of solver-aided programming. This section
describes the corresponding syntactic constructs in detail.

@declare-exporting[rosette/base/form/define 
                   rosette/query/form 
                   rosette/base/base
                   rosette/base/core/bool
                   rosette/query/finitize
                   #:use-sources 
                   (rosette/base/form/define 
                   rosette/query/form
                   rosette/base/base
                   rosette/base/core/bool
                   rosette/query/finitize)]

@section[#:tag "sec:symbolic-constants"]{Symbolic Constants}

@defform*[((define-symbolic id ...+ type)
           (define-symbolic id type #:length k))
         #:contracts
         [(type (and/c solvable? type?))
          (k natural?)]]{
                         
 The first form binds each provided identifier to a distinct
 @tech["symbolic constant"] of the given
 @tech["solvable type"]. The identifiers are bound to the
 same constants every time the form is evaluated.
  
  @examples[#:eval rosette-eval
  (define (always-same)
    (define-symbolic x integer?)
    x)
  (always-same)
  (always-same) 
  (equal? (always-same) (always-same)) ]

 The second form creates a list of @racket[k] distinct
 constants and binds it to @racket[id]. The same constants
 are bound to @racket[id] every time the form is evaluated.
 The form requires @racket[k] to evaluate to a natural number
 statically---i.e., at macro expansion time.

  @examples[#:eval rosette-eval
  (define (always-same-3)
    (define-symbolic y integer? #:length (+ 1 2))
    y)
  (always-same-3)
  (always-same-3) 
  (equal? (always-same-3) (always-same-3))
  (eval:alts
   (define (always-same-n n)
     (define-symbolic y integer? #:length n)
    y)
   (eval:error (lambda (n) (define-symbolic y integer? #:length n) y)))]
}

@defform*[((define-symbolic* id ...+ type)
           (define-symbolic* id type #:length k))
         #:contracts
         [(type (and/c solvable? type?))
          (k natural?)]]{
                         
 The first form creates a stream of distinct
 @tech["symbolic constant"]s of the given
 @tech["solvable type"] for each identifier, binding the
 identifier to the next element from its stream every time
 the form is evaluated.
  
  @examples[#:eval rosette-eval
  (define (always-different)
    (define-symbolic* x integer?)
    x)
  (always-different) 
  (always-different) 
  (eq? (always-different) (always-different))]

 The second form binds @racket[id] to a list of the next
 @racket[k] elements from its stream every time the form is
 evaluated. The expression @racket[k] may produce different
 natural numbers depending on the calling context.

  @examples[#:eval rosette-eval
  (define (always-different-n n)
    (define-symbolic* y integer? #:length n)
    y)
  (always-different-n 2)
  (always-different-n 3) 
  (equal? (always-different-n 4) (always-different-n 4))]}

@section[#:tag "sec:assertions"]{Assertions and Assumptions}

@defform*[((assert expr)
           (assert expr msg))
         #:contracts
         [(msg string?)]]{
                          
 Checks that @racket[expr] produces a true value. Rosette
 keeps track of all assertions and assumptions encountered
 during symbolic evaluation in the current @tech{verification condition} (VC).
 If @racket[expr] evaluates to @racket[#f],
 the assertion adds @racket[#f] to the VC and throws an error
 with the optional failure message @racket[msg]. If
 @racket[expr] evaluates to a symbolic boolean value, this
 value is added to the VC and execution continues. If
 @racket[expr] evaluates to any other value, @racket[assert]
 has no effect. The contents of the VC can be examined using
 the @racket[(vc)] procedure, and they can be cleared using
 the @racket[clear-vc!] procedure.

  @examples[#:eval rosette-eval
  (code:line (assert #t) (code:comment "No effect."))
  (code:line (assert 1)  (code:comment "No effect."))
  (code:line (vc)        (code:comment "The VC tracks assumptions and assertions."))  
  (define-symbolic x boolean?)
  (assert x)
  (code:line (vc)        (code:comment "x is added to the VC's asserts."))  
  (eval:error (assert #f "bad value"))
  (vc) 
  (code:line (clear-vc!) (code:comment "Clear the VC."))
  (vc)]
}

@defform*[((assume expr)
           (assume expr msg))
         #:contracts
         [(msg string?)]]{
                          
 Behaves like @racket[assert] except that it updates the
 assumption component of the current verification condition
 when @racket[expr] evaluates to @racket[#f] or to a
 symbolic boolean.

 @examples[#:eval rosette-eval
  (vc)
  (code:line (assume #t) (code:comment "No effect."))
  (code:line (assume 1)  (code:comment "No effect."))
  (code:line (vc)        (code:comment "The VC tracks assumptions and assertions."))  
  (define-symbolic x boolean?)
  (assume x)
  (code:line (vc)        (code:comment "x is added to the VC's assumes."))  
  (eval:error (assume #f "bad value"))
  (vc) 
  (code:line (clear-vc!) (code:comment "Clear the VC."))
  (vc)]
}



@section{Verification}

@defform[(verify expr)]{

 Searches for a binding of symbolic constants to concrete
 values that satisfies all the assumptions and violates at
 least one of the assertions encountered during the
 evaluation of @racket[expr]. The binding must also satisfy
 all the assumptions and assertions accumulated in the
 verification condition @racket[(vc)] before the call to
 @racket[verify]. If such a binding exists, the query returns
 one as part of a satisfiable @racket[solution?]; otherwise,
 the query returns @racket[(unsat)].

 Formally,
 @racket[(verify expr)] searches for a model of the formula
 @racket[(vc-assumes #, @var{P})] ∧ @racket[(vc-asserts #, @var{P})] ∧
 @racket[(vc-assumes #, @var{Q})] ∧ ¬ @racket[(vc-asserts #, @var{Q})],
 where @var{P} is the verification condition before the
 call to @racket[verify] and @var{Q} is the verification condition
 generated by evaluating @racket[expr].

 The @racket[verify]
 query does not retain the assumptions and assertions generated
 by @racket[expr], leaving the current verification condition
 @racket[(vc)] unchanged after the query returns.

  @examples[#:eval rosette-eval
  (define-symbolic a b c d boolean?)
  (vc)
  (code:comment "This query forces a to be false:")
  (verify (assert a)) 
  (code:line (vc) (code:comment "VC is unchanged."))
  (assume a)           
  (assert b)           
  (vc)
  (code:comment "This query forces a, b, c to be true, and d to be false:")
  (verify 
   (begin
     (assume c)  
     (assert d)))
  (vc)
  (code:comment "This query has no solution because we assumed a above:")
  (verify (assert a))
  (code:comment "Clearing the VC gives the expected solution:")
  (clear-vc!)
  (vc)
  (verify (assert a))] 
}

@(rosette-eval '(clear-vc!))

@section{Synthesis}


@defform*[((synthesize input expr)
           (synthesize #:forall input #:guarantee expr))]{

 Taking @var{I} to be the set of symbolic constants @racket[(symbolics input)] 
 and @var{H} to be the complement of @var{I}, 
 searches for a binding @var{B}@subscript{@var{H}}
 from @var{H} to concrete values that 
 satisfies @racket[expr] as follows.
 If @var{B}@subscript{@var{H}} is extended with any binding
 @var{B}@subscript{@var{I}} from @var{I}
 to concrete values, then the extended binding
 @var{B}@subscript{@var{H}} ∪ @var{B}@subscript{@var{I}}
 satisfies all the assertions generated by evaluating @racket[expr] whenever
 it satisfies the assumptions generated by @racket[expr], as
 well as the assumptions and assertions accumulated in the verification condition
 @racket[(vc)] before @racket[expr] is evaluated. If such a binding
 @var{B}@subscript{@var{H}} exists, the query returns one as part of a
 satisfiable @racket[solution?]; otherwise, the query returns
 @racket[(unsat)].

 Formally, @racket[(synthesize input expr)] searches for a model of 
 the formula
 ∃ @var{H}. (∃ @var{I}. @var{pre}(@var{H}, @var{I})) ∧
 (∀ @var{I}. @var{pre}(@var{H}, @var{I}) ⇒ @var{post}(@var{H}, @var{I})),
 where @var{pre}(@var{H}, @var{I}) is @racket[(vc-assumes #, @var{P})] ∧
 @racket[(vc-asserts #, @var{P})] ∧ @racket[(vc-assumes #, @var{Q})],
 @var{post}(@var{H}, @var{I}) is @racket[(vc-asserts #, @var{Q})],
 @var{P} is the verification condition accumulated before the evaluation of 
 @racket[expr], and @var{Q} is the verification condition
 generated by evaluating @racket[expr]. This formula is stronger than the
 classic synthesis formula
 ∃ @var{H}. ∀ @var{I}. @var{pre}(@var{H}, @var{I}) ⇒ @var{post}(@var{H}, @var{I}).
 The additional constraint, ∃ @var{I}. @var{pre}(@var{H}, @var{I}), rules out
 trivial solutions that allow @var{pre}(@var{H}, @var{I}) to 
 be false on all inputs @var{I}. 
 The formulas @var{pre}(@var{H}, @var{I}) and
 @var{post}(@var{H}, @var{I}) are required to be free of quantifiers, so
 no @tech[#:key "quantified formula"]{quantified formulas} can be part of 
 the assumptions or assertions that make up a synthesis query.
 
 The @racket[synthesize] query does not retain the
 assumptions and assertions generated by @racket[expr],
 but it does retain the updates to @racket[(vc)], if any,
 produced by evaluating @racket[input]. In other words,
 @racket[(synthesize input expr)] is equivalent to
 @racket[(let ([v input]) (synthesize v expr))], where @racket[v]
 is a fresh variable. 
  
  @examples[#:eval rosette-eval
  (define-symbolic x c integer?)
  (vc)
  (code:comment "This query finds a binding for c that works for all even x:")
  (synthesize
     #:forall (list x) 
     #:guarantee
     (begin
       (assume (even? x))
       (assert (odd? (+ x c)))))
  (code:line (vc)   (code:comment "VC is unchanged."))
  (assume (odd? x))
  (vc)
  (code:comment "This query finds a binding for c that works for all odd x:")
  (synthesize
   #:forall    (list x)
   #:guarantee (assert (odd? (+ x c))))
  (vc)
  (code:comment "This query has no solution because we assumed (odd? x) above:")
  (synthesize
   #:forall (list x)
   #:guarantee
   (begin
     (assume (even? x))
     (assert (odd? (+ x c)))))
  (code:comment "Clearing the VC gives the expected solution:")
  (clear-vc!)
  (vc)
  (synthesize
   #:forall (list x)
   #:guarantee
   (begin
     (assume (even? x))
     (assert (odd? (+ x c)))))]
}

@(rosette-eval '(clear-vc!))

@section{Angelic Execution}

@defform[(solve expr)]{
                       
 Searches for a binding of symbolic constants to concrete
 values that satisfies all the assumptions and assertions
 encountered during the evaluation of @racket[expr], as well
 as all the assumptions and assertions accumulated in the
 verification condition @racket[(vc)] before the call to
 @racket[solve]. If such a binding exists, the query returns
 one as part of a satisfiable @racket[solution?]; otherwise,
 the result is an unsatisfiable solution.
 
 Formally,
 @racket[(solve expr)] searches for a model of the formula
 @racket[(vc-assumes #, @var{P})] ∧ @racket[(vc-asserts #, @var{P})] ∧
 @racket[(vc-assumes #, @var{Q})] ∧ @racket[(vc-asserts #, @var{Q})],
 where @var{P} is the verification condition before the
 call to @racket[solve] and @var{Q} is the verification condition
 generated by evaluating @racket[expr].

 The @racket[solve]
 query does not retain the assumptions and assertions generated
 by @racket[expr], leaving the current verification condition
 @racket[(vc)] unchanged after the query returns.
 
 @examples[#:eval rosette-eval
 (define-symbolic x y boolean?)
 (assume x)
 (code:line (vc)   (code:comment "x is added to the VC's assumes."))
 (code:comment "This query forces both x and y to be true.")
 (solve (assert y))
 (code:line (vc)   (code:comment "VC is unchanged."))
 (code:comment "This query has solution because we assumed (not x) above:")
 (solve (assert (not x)))
 (code:comment "Clearing the VC gives the expected solution:")
 (clear-vc!)
 (vc)
 (solve (assert (not x)))]
}

@(rosette-eval '(clear-vc!))

@defproc[(solve+) procedure?]{
Returns a stateful procedure that uses a fresh @racket[solver?] instance
to incrementally solve a sequence of constraints.

The returned procedure consumes a constraint (i.e., a boolean value or @tech["symbolic term"]),
a positive integer, or the symbol @racket['shutdown].

If the argument is a constraint, it is pushed onto the solver's constraint stack and
a solution for all constraints on the stack is returned.

If the argument is a positive integer @var[k], then the top @var[k] constraints are popped
from the solver's constraint stack and the result is the solution to the remaining constraints.

If the argument is @racket['shutdown], all resources used by the procedure are released, and any
subsequent calls to the procedure throw an exception.
 @examples[#:eval rosette-eval
  (define-symbolic x y integer?)
  (define inc (solve+))
  (code:line (inc (< x y))   (code:comment "Push (< x y) and solve."))
  (code:line (inc (> x 5))   (code:comment "Push (> x 5) and solve."))
  (code:line (inc (< y 4))   (code:comment "Push (< y 4) and solve."))
  (code:line (inc 1)         (code:comment "Pop  (< y 4) and solve."))
  (code:line (inc (< y 9))   (code:comment "Push (< y 9) and solve."))
  (code:line (inc 'shutdown) (code:comment "Release resources."))
  (eval:alts
   (code:line (inc (> y 4))   (code:comment "Unusable after shutdown."))
   (eval:error (inc (> y 4))))]
}


@(rosette-eval '(clear-vc!))

@section{Optimization}

@defform[(optimize
            maybe-minimize
            maybe-maximize
            #:guarantee expr)
          #:grammar ([maybe-minimize (code:line) (code:line #:minimize min-expr)]
                     [maybe-maximize (code:line) (code:line #:maximize max-expr)])
          #:contracts [(min-expr (listof (or/c integer? real? bv?)))
                       (max-expr (listof (or/c integer? real? bv?)))]]{

 Searches for an optimal binding of symbolic constants to
 concrete values that satisfies all the assumptions and
 assertions encountered during the evaluation of
 @racket[expr], as well as all the assumptions and assertions
 accumulated in the verification condition @racket[(vc)]
 before the evaluation of @racket[expr]. The binding is
 optimal in that it minimizes the cost terms in
 the @racket[min-expr] list and maximizes those in the
 @racket[max-expr] list.  If such a binding exists, the query
 returns one as part of a satisfiable @racket[solution?]; otherwise,
 the query returns @racket[(unsat)]. For more details on
 solving optimization problems, see the
 @hyperlink["https://rise4fun.com/Z3/tutorial/optimization"]{Z3 optimization tutorial}.

 
 Formally,
 @racket[(solve expr)] searches for an optimal model of the formula
 @racket[(vc-assumes #, @var{P})] ∧ @racket[(vc-asserts #, @var{P})] ∧
 @racket[(vc-assumes #, @var{Q})] ∧ @racket[(vc-asserts #, @var{Q})],
 where @var{P} is the verification condition before the
 evaluation of @racket[expr], @var{Q} is the verification condition
 generated by evaluating @racket[expr], the cost terms @racket[min-expr] are
 minimized, and the cost terms @racket[max-expr] are maximized.

 The @racket[optimize] query does not retain the
 assumptions and assertions generated by @racket[expr],
 but it does retain the updates to @racket[(vc)], if any,
 produced by evaluating @racket[min-expr] and @racket[max-expr]. In other words,
 @racket[(optimize #:minimize min-expr #:maximize max-expr #:guarantee expr)] is
 equivalent to
 @racket[(let ([v min-expr] [w max-expr]) (optimize #:minimize v #:maximize w #:guarantee expr))],
 where @racket[v] and @racket[w] are fresh variables.
  
  @examples[#:eval rosette-eval
  (define-symbolic x y integer?)
  (code:comment "This query maximizes x + y while ensuring that y - x < 1 whenever x < 2:")
  (optimize
   #:maximize (list (+ x y))
   #:guarantee
   (begin
     (assume (< x 2))
     (assert (< (- y x) 1))))]
}

@(rosette-eval '(clear-vc!))
@(rosette-eval '(current-bitwidth 5))

@section[#:tag "sec:reasoning-precision"]{Reasoning Precision}

@defparam[current-bitwidth k (or/c #f positive-integer?)
          #:value #f]{
  A parameter that defines the current @deftech[#:key "reasoning precision"]{reasoning precision}
  for solver-aided queries over @racket[integer?] and @racket[real?] constants.
  Setting @racket[current-bitwidth] to a positive integer @racket[k] instructs Rosette to approximate
  both reals and integers with signed @racket[k]-bit words. Setting it to @racket[#f] instructs Rosette to use
  infinite precision for real and integer operations.  As a general rule, @racket[current-bitwidth] should
  be set once, before any solver-aided queries are issued. 

  When  @racket[current-bitwidth] is @racket[#f], Rosette translates queries over
  reals and integers into constraints in the
  @hyperlink["http://rise4fun.com/z3/tutorial"]{theories of reals and integers}. 
  These theories are effectively decidable only for linear constraints,
  so setting @racket[current-bitwidth] to a positive integer will lead to better
  performance for programs that perform nonlinear arithmetic.

  When @racket[current-bitwidth] is set to a positive integer @racket[k],
  Rosette translates queries over reals and integers into constraints in the 
  @hyperlink["http://rise4fun.com/z3/tutorial"]{theory of bitvectors}
  (of size @racket[k]), which can be decided efficiently in practice.
  When this form of translation is used, however, solver-aided queries can produce
  counterintuitive results due to arithmetic over- and under-flow, as demonstrated below.

  Rosette sets @racket[current-bitwidth] to @racket[#f] by default for two reasons.
  First, this setting is consistent with Racket's infinite-precision semantics for integers and reals,
  avoiding counterintuitive query behavior.
  Second, the @racket[current-bitwidth] parameter must be set to @racket[#f] when
  executing queries over assertions that contain @tech[#:key "quantified formula"]{quantified formulas}
  or @seclink["sec:UF"]{uninterpreted functions}.
  Otherwise, such a query will throw an exception.

  @examples[
 #:eval rosette-eval
 (define-symbolic x y real?)
 (define-symbolic f (~> real? real?))
 (code:line (current-bitwidth 5)                    (code:comment "Use 5 bits for integers and reals."))
 (code:line (solve (assert (= x 3.5)))              (code:comment "3.5 = 3 under 5-bit semantics."))
 (code:line (solve (assert (= x 64)))               (code:comment "0 = 64 under 5-bit semantics,"))
 (code:line (solve (assert (and (= x 64) (= x 0)))) (code:comment "leading to counterintuitive results."))
 (eval:alts
  (solve                                  (code:comment "Quantifiers are not supported,")
   (assert (forall (list x) (= x (+ x y)))))
  (eval:error (solve (assert (forall (list x) (= x (+ x y)))))))
 (eval:alts
  (solve                                  (code:comment "and neither are uninterpreted functions.")
   (assert (= x (f x))))  
  (eval:error (solve (assert (= x (f x))))))
 (code:line (current-bitwidth #f)                   (code:comment "Use infinite-precision semantics ..."))
 (code:line (solve (assert (= x 3.5)))              (code:comment "to obtain the expected results."))
 (solve (assert (= x 64)))
 (solve (assert (and (= x 64) (= x 0))))
 (solve                                  (code:comment "Quantifiers work, and")
  (assert (forall (list x) (= x (+ x y)))))
 (code:line (solve (assert (= x (f x))))            (code:comment "so do uninterpreted functions."))
 (define-symbolic i j integer?)
 (solve                                  (code:comment "But nonlinear integer arithmetic")
  (begin                                 (code:comment "is undecidable.")
    (assert (> i 0))
    (assert (> j 0))
    (assert (or (= (/ i j) 2) (= (/ j i) 2)))))]
}

 


@(kill-evaluator rosette-eval)
