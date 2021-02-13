#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/query rosette/solver/solution
           rosette/base/core/term (only-in rosette/query/finitize current-bitwidth)
           (only-in rosette/base/core/union union?)
           (only-in rosette/base/core/function ~> function? fv?)
           (only-in rosette/base/base bv bitvector assert vc clear-vc!))
          (for-label racket) racket/runtime-path
          scribble/core scribble/html-properties scribble/examples racket/sandbox
          "../util/lifted.rkt")

@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "uninterpreted-log")))

@title[#:tag "sec:UF"]{Uninterpreted Functions}

@declare-exporting[rosette/base/base #:use-sources (rosette/base/core/function
                                                    rosette/query/finitize
                                                    rosette/base/base)]

In Rosette, functions are special kinds of @seclink["sec:proc"]{procedures} that are pure
(have no side effects) and total (defined on every input value).
A function type is recognized by the @racket[function?] predicate, and all
function types are @tech[#:key "solvable type"]{solvable}.  The type of a
function specifies the function's domain and range, which are given as @racket[solvable?] non-@racket[function?] types.  A value of a function type is recognized by
the @racket[fv?] (function value) predicate. Because
function types are solvable, they can be used in the  @seclink["sec:symbolic-constants"]{@code{define-symbolic[*]}} form
to introduce a symbolic function constant. These symbolic function constants are
technically @deftech[#:key "uninterpreted function"]{uninterpreted functions}---they have
no fixed meaning.  Their meaning (or interpretation) is determined by the underlying solver
as the result of a @seclink["sec:queries"]{solver-aided query}.

@examples[#:eval rosette-eval
(current-bitwidth #f)
(code:comment "An uninterpreted function from integers to booleans:")
(define-symbolic f (~> integer? boolean?))
(code:line (f 1)     (code:comment "No built-in interpretation for 1."))
(define-symbolic x real?)
(code:line (f x)     (code:comment "This typechecks when x is an integer,"))
(code:line (vc)      (code:comment "so Rosette emits the corresponding assertion."))
(define sol (solve (assert (not (equal? (f x) (f 1))))))
(code:line (define g (evaluate f sol)) (code:comment "An interpretation of f."))
g
(evaluate x sol)
(code:line (fv? f)   (code:comment "f is a function value,"))
(code:line (fv? g)   (code:comment "and so is g."))
(code:line (g 2)     (code:comment "We can apply g to concrete values"))
(code:line (g x)     (code:comment "and to symbolic values."))] 

@defproc[(~> [d (and/c solvable? (not/c function?))] ...+
             [r (and/c solvable? (not/c function?))]) function?]{
                                                                 
  Returns a type predicate for recognizing functions that take as input
  values of types @racket[d...+] and produce values of type @racket[r].
  The domain and range arguments must be concrete @racket[solvable?] types that are
  not themselves functions. Note that @racket[~>] expects at least one domain
  type to be given, disallowing zero-argument functions.
  
  @examples[#:eval rosette-eval
  (define t (~> integer? real? boolean? (bitvector 4)))
  t
  (eval:error (~> t integer?))
  (define-symbolic b boolean?)
  (eval:error (~> integer? (if b boolean? real?)))
  (eval:error (~> real?))]
}

@defproc[(function? [v any/c]) boolean?]{
                                         
  Returns true if @racket[v] is a concrete type predicate that recognizes function values.
                  
  @examples[#:eval rosette-eval
  (define t0? (~> integer? real? boolean? (bitvector 4)))
  (define t1? (~> integer? real?))
  (function? t0?)
  (function? t1?)
  (define-symbolic b boolean?)
  (code:line (function? (if b t0? t1?)) (code:comment "Not a concrete type."))
  (code:line (function? integer?)       (code:comment "Not a function type."))
  (code:line (function? 3)              (code:comment "Not a type."))]
}

@(rosette-eval '(clear-vc!))

@defproc[(fv? [v any/c]) boolean?]{
                                   
  Returns true if @racket[v] is a concrete or symbolic function value.
                  
  @examples[#:eval rosette-eval
  (define-symbolic f (~> boolean? boolean?))
  (fv? f)
  (fv? (lambda (x) x))
  (define-symbolic b boolean?)
  (fv? (if b f 1))
  (define sol
    (solve
     (begin
       (assert (not (f #t)))
       (assert (f #f)))))
  (define g (evaluate f sol))
  (code:line g (code:comment "g implements logical negation."))
  (fv? g)
  (code:comment "Verify that g implements logical negation:")
  (verify (assert (equal? (g b) (not b))))]
}


@(kill-evaluator rosette-eval)