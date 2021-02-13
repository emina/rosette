#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/solver/solution rosette/query/query 
           rosette/base/core/term rosette/lib/angelic
           rosette/lib/synthax 
           (only-in rosette/base/base assert function? bitvector
                    bvshl bvashr bvlshr bvadd bvsub bvmul bvsdiv
                    bvand bvor bvxor bvult bvule bvslt bvsle)
           (only-in rosette/lib/destruct destruct)
           racket)
          scribble/core scribble/html-properties scribble/examples racket/sandbox  racket/runtime-path
          "../util/lifted.rkt")


@(define-runtime-path root ".")
@(define rosette-eval (rosette-log-evaluator (logfile root "libs-log")))


@title[#:tag "sec:rosette-libs"]{Solver-Aided Libraries}

In principle, solver-aided programming requires only
symbolic values and the basic constructs described in
Chapter @seclink["ch:syntactic-forms:rosette"]{3}. In
practice, however, it is often convenient to work with
richer constructs, which are built on top of these
primitives. Rosette ships with two libraries that provide
such constructs, as well as utility procedures for turning
the results of synthesis queries into code.


@section{Synthesis Library}

@defmodule[rosette/lib/synthax #:use-sources (rosette/lib/synthax)]

@(rosette-eval '(require rosette/lib/synthax))
@(rosette-eval '(require (only-in rosette/guide/scribble/util/demo print-forms-alt)))

@defform[(?? maybe-type)
         #:grammar [(maybe-type (code:line)
                                type-expr)]
         #:contracts ([type-expr (and/c solvable? type? (not/c function?))])                                  
         ]{
           
 Introduces a constant @tech{hole} into a program---a
 placeholder for a concrete constant of the given type. The
 default value for @racket[type-expr], if one is not
 provided, is @racket[integer?]. The @racket[??] construct
 @seclink["sec:symbolic-constants"]{creates} and returns a
 distinct symbolic constant of type @racket[type-expr].

 When used in a recursive @racketlink[define-grammar]{grammar definition},
 a @racket[??] hole generates a fresh constant
 every time it is evaluated, similarly to
 @racket[define-symbolic*]. Otherwise, a @racket[??] hole
 always generates the same constant, similarly to
 @racket[define-symbolic].

 See also @racket[define-grammar].

@examples[#:eval rosette-eval #:no-prompt
(code:comment "Uses a ?? hole to sketch a procedure for multiplying")
(code:comment "an 8-bit number by 2 using a constant left shift.")
(eval:alts
(define (bvmul2_?? x) 
  (bvshl x (?? (bitvector 8))))
(require (only-in rosette/guide/scribble/libs/bvmul2 bvmul2_??)))]
@examples[#:eval rosette-eval #:label #f
(code:comment "bvmul2_?? hole always returns the same constant:")
(bvmul2_?? (bv 1 8))
(bvmul2_?? (bv 3 8))

(define-symbolic x (bitvector 8))
(bvmul2_?? x)
(equal? (bvmul2_?? x) (bvmul2_?? x))

(code:comment "Use synthesis to fill the hole with a constant:")
(define sol
  (synthesize
   #:forall (list x)
   #:guarantee (assert (equal? (bvmul2_?? x) (bvmul x (bv 2 8))))))
sol
(code:comment "Save bvmul2_?? to a file before calling print-forms.")
(eval:alts (print-forms sol) (print-forms-alt sol))
]
}


@defform[(choose expr ...+)]{
                             
 Introduces a choice @tech{hole} into a program---a
 placeholder to be filled with one of the given expressions.
 This construct defines @var[n]-1 distinct boolean constants
 and uses them to conditionally select one of the @var[n]
 provided expressions.

 When used in a recursive @racketlink[define-grammar]{grammar definition},
 a @racket[choose] hole generates fresh selector
 constants and may select a different expression every time
 it is evaluated. Otherwise, a @racket[choose] hole generates
 the same constants and makes the same selection every time.

 See also @racket[choose*] and @racket[define-grammar].

@examples[#:eval rosette-eval  #:no-prompt
(code:comment "Uses a chooose hole to sketch a procedure for multiplying")
(code:comment "an 8-bit number by 2 using a constant shift.")
(eval:alts
(define (bvmul2_choose x)
  ((choose bvshl bvashr bvlshr) x (?? (bitvector 8))))
(require (only-in rosette/guide/scribble/libs/bvmul2 bvmul2_choose)))]
@examples[#:eval rosette-eval #:label #f
(code:comment "bvmul2_choose holes always generate the same constants:")
(define-symbolic x (bitvector 8))
(bvmul2_choose x)
(equal? (bvmul2_choose x) (bvmul2_choose x))

(code:comment "Use synthesis to fill the holes:")
(define sol
  (synthesize
   #:forall (list x)
   #:guarantee (assert (equal? (bvmul2_choose x) (bvmul x (bv 2 8))))))
sol
(code:comment "Save bvmul2_choose to a file before calling print-forms.")
(eval:alts (print-forms sol) (print-forms-alt sol))
]

}



@defform[(define-grammar (id arg-id ...)
           [rule-id rule-expr] ...+)]{
                                     
 Defines a macro for creating and filling grammar
 @tech[#:key "hole"]{holes}. The macro uses
 @racket[(id arg-id ...)] as the pattern for creating holes
 and @racket[[rule-id rule-expr] ...+] as the rules for
 filling these holes with expressions. In particular, writing
 @racket[(id #, @var[arg-expr] ...)] creates a hole for an
 expression derived from the grammar
 @racket[[rule-id rule-expr] ...+], where the pattern
 variables @racket[arg-id] are instantianted with the hole
 arguments @racket[#, @var[arg-expr]].

 Each hole defines a local scope that binds the rule names
 @racket[rule-id] to procedures of the form
 @racket[(lambda () ... rule-expr)]. This scope behaves like
 @racket[letrec], so rules can refer to themselves and each
 other via @racket[(rule-id)] to specify a recursive grammar.

 For example, the following definition specifies a
 grammar of fast bitvector expressions over one input:

 @(rosette-eval '(clear-vc!))
 @examples[#:eval rosette-eval #:label #f #:no-prompt
 (code:line
  (define-grammar (bitfast y)         
    [expr                            (code:comment "<expr> :=")
     (choose y (?? (bitvector 8))    (code:comment " y | <8-bit constant> |")
             ((bop) (expr) (expr)))] (code:comment " ((bop) <expr> <expr>)")
    [bop                             (code:comment "<bop>  :=")
     (choose bvshl bvashr bvlshr     (code:comment " bvshl | bvashr | bvlshr |")
             bvand bvor bvxor        (code:comment " bvand | bvor | bvxor | ")
             bvadd bvsub)])          (code:comment " bvadd | bvsub"))]

 By default, a grammar hole is filled by evaluating the
 first grammar rule @racket[(#, @elem{@var{rule-id}@subscript{0}})]
 in the hole's local scope, and limiting the call stack to include at
 most @racket[(current-grammar-depth)] applications of the grammar rules.
 Holes can override these defaults by providing the optional arguments  
 @racket[#:depth n] and @racket[#:start #, @elem{@var{rule-id}@subscript{k}}], 
 which control the call stack depth and the starting rule, respectively.

 @examples[#:eval rosette-eval #:label #f #:no-prompt
 (code:comment "Uses a bitfast hole to sketch a procedure consisting")
 (code:comment "of a fast bitvector expression over x, created by at")
 (code:comment "most 2 applications of the bitfast grammar rules,")
 (code:comment "following the initial application of the (expr) rule.")
 (eval:alts
  (define (bvmul2_bitfast x)
    (bitfast x #:depth 2))
  (require (only-in rosette/guide/scribble/libs/bvmul2 bvmul2_bitfast)))]
 
 Whenever a grammar hole is evaluated, its constituent
 @racket[choose] and @racket[??] holes generate the same set
 of constants, with each rule application generating a
 distinct subset of this set. This allows different
 applications of the same rule to produce different
 components of the unique expression that fills a grammar
 hole. 
 
@examples[#:eval rosette-eval #:label #f
 (code:comment "The choose and ?? holes that are part of the same bitfast")
 (code:comment "hole always generate the same set of constants:")
 (define-symbolic x (bitvector 8))
 (eval:alts
  (equal? (bvmul2_bitfast x) (bvmul2_bitfast x))
  (begin0
    (equal? (bvmul2_bitfast x) (bvmul2_bitfast x))
    (clear-vc!)))
    

 (define sol
    (synthesize
     #:forall (list x)
     #:guarantee (assert (equal? (bvmul2_bitfast x) (bvmul x (bv 2 8))))))

 (code:comment "Save bvmul2_bitfast to a file before calling print-forms.")
 (eval:alts (print-forms sol) (print-forms-alt sol))]

 Grammar rules can include grammar holes as part of their
 definition. An included grammar hole behaves just like an
 included @racket[choose] or @racket[??] hole: it can be
 filled with different expressions in different applications
 of the enclosing rule.

 @(rosette-eval '(clear-vc!))
 @examples[#:eval rosette-eval #:label #f #:no-prompt
 (code:comment "Specifies a grammar of signed and unsigned")
 (code:comment "comparisons of bitfast expressions.")
 (code:line
  (define-grammar (bitcmp y)         
    [cmp                            
     (choose                         (code:comment "<cmp> :=")
      ((op) (bitfast y) (bitfast y)) (code:comment " (<op> <bitfast y> <bitfast y>) |")
      (and (cmp) (cmp)))]            (code:comment " (and <cmp> <cmp>)")
    [op
     (choose                         (code:comment "<op>  :=")
      bvult bvule                    (code:comment " bvult | bvule |")
      bvslt bvsle)])                 (code:comment " bvslt | bvsle"))

  (code:comment "Uses bitcmp and bitfast holes to sketch")
  (code:comment "a procedure with a conditional, intended")
  (code:comment "to implement fast signed division by 2.")
  (eval:alts
   (define (bvsdiv2_bitcmp x)
     (if (bitcmp x)
         (bitfast x)
         (bitfast x)))
   (require (only-in rosette/guide/scribble/libs/bvmul2 bvsdiv2_bitcmp)))]
 @examples[#:eval rosette-eval #:label #f
  (define-symbolic x (bitvector 8))
  (code:line (current-grammar-depth 2) (code:comment "Set the grammar depth."))
  (define sol
    (synthesize
     #:forall (list x)
     #:guarantee (assert (equal? (bvsdiv2_bitcmp x) (bvsdiv x (bv 2 8))))))
  (code:comment "Save bvsdiv2_bitcmp to a file before calling print-forms.")
  (code:comment "Note that the solver can return any correct solution.")
  (code:comment "In this case, there is a shorter solution that omits the")
  (code:comment "trivially true right child from the test expression.")
  (eval:alts (print-forms sol) (print-forms-alt sol))]
 

 Finally, grammar holes can accept other holes as arguments.
 Unlike included holes, argument holes are @emph{shared}
 within the same rule. If an argument @racket[arg-id] is
 bound to a hole @var[arg-expr], then @emph{all} occurrences
 of @racket[arg-id] in a given rule refer to @emph{one}
 shared @var[arg-expr] hole, which can be filled with
 different expressions in different applications of the rule.
 In contrast, if a rule explicitly includes @var[arg-expr]
 multiple times, then each occurrence of @var[arg-expr]
 creates a distinct hole. The following examples illustrate
 the difference and show how to structure grammars to avoid
 unwanted sharing of argument holes.

 @(rosette-eval '(clear-vc!))
 @examples[#:eval rosette-eval #:label #f #:no-prompt
 (code:comment "This grammar is intended to take a hole xs")
 (code:comment "as an argument and use it to define the space")
 (code:comment "of signed and unsigned comparisons over the xs")
 (code:comment "expressions. The grammar is the same as bitcmp")
 (code:comment "except that it uses xs instead of (bitfast y)")
 (code:comment "in the cmp rule.")
 (define-grammar (bvcmp xs)
  [cmp
   (choose              (code:comment "Both occurrences of xs")
    ((op) xs xs)        (code:comment "refer to a single shared")
    (and (cmp) (cmp)))] (code:comment "hole in this rule.")
  [op
   (choose
    bvult bvule
    bvslt bvsle)])
 (code:comment "This sketch is the same as bvsdiv2_bitcmp except")
 (code:comment "that it replaces bitcmp with (bvcmp (bitfast x)).")
 (eval:alts
   (define (bvsdiv2_bvcmp x)
     (if (bvcmp (bitfast x))
         (bitfast x)
         (bitfast x)))
   (require (only-in rosette/guide/scribble/libs/bvmul2 bvsdiv2_bvcmp)))]
 @examples[#:eval rosette-eval #:label #f
  (define-symbolic x (bitvector 8))
  (current-grammar-depth 2)
  (code:comment "There is no solution because the two xs in the")
  (code:comment "cmp rule of the bvcmp grammar refer to the same")
  (code:comment "hole, so it is not possible for the rule to")
  (code:comment "create a comparison expression with different left")
  (code:comment "and right children (comparing x to a constant).")
  (synthesize
    #:forall (list x)
    #:guarantee (assert (equal? (bvsdiv2_bvcmp x) (bvsdiv x (bv 2 8)))))]
 @examples[#:eval rosette-eval #:label #f #:no-prompt
 (code:comment "To avoid the sharing, we refactor bvcmp to place xs into")
 (code:comment "its own rule, arg, which is then called by cmp. ")
 (code:comment "This works because every application of the arg rule")
 (code:comment "can fill its sole xs hole with a different expression.")
 (define-grammar (bvcmp* xs)
  [cmp
   (choose
    ((op) (arg) (arg))
    (and (cmp) (cmp)))]
  [op
   (choose
    bvult bvule
    bvslt bvsle)]
  [arg xs])
 (code:comment "This sketch is the same as bvsdiv2_bitcmp except")
 (code:comment "that it replaces bitcmp with (bvcmp* (bitfast x)).")
 (eval:alts
   (define (bvsdiv2_bvcmp* x)
     (if (bvcmp* (bitfast x))
         (bitfast x)
         (bitfast x)))
   (require (only-in rosette/guide/scribble/libs/bvmul2 bvsdiv2_bvcmp*)))]
  @(rosette-eval '(clear-vc!))
  @examples[#:eval rosette-eval #:label #f
  (define-symbolic x (bitvector 8))
  (current-grammar-depth 2)
  (code:comment "A solution exists now because the arg rule produces")
  (code:comment "different left and right children for the comparison")
  (code:comment "operator used in the bvsdiv2_bvcmp* conditional test.")
  (define sol
    (synthesize
     #:forall (list x)
     #:guarantee (assert (equal? (bvsdiv2_bvcmp* x) (bvsdiv x (bv 2 8))))))
  (code:comment "Save bvsdiv2_bvcmp* to a file before calling print-forms.")
  (eval:alts (print-forms sol) (print-forms-alt sol))]}

@defform[(define-simple-grammar (id arg-id ...) body)]{
 Defines a grammar with a single (possibly recursive) rule. Equivalent to
 @(racketblock
   (define-grammar (id param ...)
     [id body]))}

@defparam[current-grammar-depth n natural?
          #:value 0]{
                     
 A parameter that controls the evaluation of
 @racketlink[define-grammar]{grammar} holes. Evaluating a
 grammar hole involves evaluating the rules of its grammar.
 The @racket[(current-grammar-depth)] parameter controls how
 many of these rules can appear on the call stack after the
 start rule is evaluated. The default value of 0 allows
 only the start rule to be evaluated, and a value of
 @racket[n > 0] allows at most @racket[n] additional rules to
 be added to the call stack. For best performance, keep the
 value of this parameter as low as possible.}                   

@defproc[(generate-forms [solution solution?]) (listof syntax?)]{
                                                                 
  Given a satisfiable @racket[solution?] to a
  @racket[synthesize] query, returns a list of @tech{sketch}
  completions for that query. Sketch completions can only be
  generated for programs that have been saved to disk.

  This procedure cooperates with the constructs in the
  @racket[rosette/lib/synthax] library to turn solutions into
  code. It works by scanning program files for
  @racketlink[??]{constant}, @racketlink[choose]{choice}, and
  @racketlink[define-grammar]{grammar} holes, and replacing
  all identified holes with code. The code for a hole is
  computed from the @racket[solution] bindings for the
  symbolic constants generated by the hole. Holes are
  identified by their original names: @racket[??],
  @racket[choose], and any @racket[id] introduced by
  @racket[define-grammar] or @racket[define-simple-grammar].
  If a program uses different names for these holes (e.g., by
  renaming them in a @racket[require] clause),
  @racket[generate-forms] will not recognize or replace them,
  even if the given solution has bindings for their symbolic
  constants.}

@defproc[(print-forms [solution solution?]) void?]{
  Pretty-prints the result of applying @racket[generate-forms] to the given  
  @racket[solution]. Sketch completions can only be generated and printed
 for programs that have been saved to disk. 
}

@section{Angelic Execution Library}
@defmodule[rosette/lib/angelic #:use-sources (rosette/lib/angelic)]

@defproc[(choose* [v any/c] ...+) any/c]{

                                         
 Symbolically chooses one of the provided values. The
 procedure creates @var[n]-1 fresh symbolic boolean constants
 every time it is evaluated, and uses these constants to
 conditionally select one of the @var[n] provided
 expressions.

 The @racket[choose*] procedure is related to the
 @racket[choose] construct from the synthesis library.
 Semantically, the difference between @racket[choose*] and
 @racket[choose] is analogous to the difference between
 @racket[define-symbolic*] and @racket[define-symbolic].
 
 They also differ in how they are typically used:
 @racket[choose] is used primarily for synthesis, while
 @racket[choose*] is suitable for both synthesis (via the
 @racket[synthesize] query) and angelic execution (via the
 @racket[solve] query).
 
 In the context of synthesis, @racket[choose] is used to
 sketch programs in (a subset of) the Rosette language, and
 @racket[choose*] can be used to sketch programs in any
 language that has a Rosette-based interpreter.
 

 @examples[#:eval rosette-eval #:no-prompt
 (require rosette/lib/angelic rosette/lib/destruct)

 (define BV (bitvector 8))
 
 (code:comment "A toy language for programs that manipulate a")
 (code:comment "a single storage cell by adding a constant to")
 (code:comment "it, multiplying it by constant, and squaring it.")
 
 (struct Add (arg) #:transparent)
 (struct Mul (arg) #:transparent)
 (struct Sqr ()    #:transparent)

 (define (interpret prog [acc (bv 0 BV)])
   (if (null? prog)
       acc
       (interpret
        (cdr prog)
        (destruct (car prog)
          [(Add v) (bvadd acc v)]
          [(Mul v) (bvmul acc v)]
          [(Sqr)   (bvmul acc acc)]
          [_       acc]))))

 (code:comment "Creates a symbolic instruction (a hole) in the toy language.")
 (define (inst*)
  (define-symbolic* arg BV)
  (choose* (Add arg) (Mul arg) (Sqr)))

 (code:comment "Creates a toy program consisting of n symbolic instructions.")
 (define (prog* n)
  (if (<= n 0)
      (list)
      (cons (inst*) (prog* (- n 1)))))]
 
 @examples[#:eval rosette-eval #:label #f
 (define-symbolic acc BV)
 (define prog (prog* 3))
 prog
 (code:comment "Complete prog so that it implements 3 * acc^2 - 1 for all acc.")
 (define sol
  (synthesize
   #:forall (list acc)
   #:guarantee
   (assert
    (equal? (interpret prog acc)
            (bvsub (bvmul (bv 3 BV) acc acc) (bv 1 BV))))))
 (evaluate prog sol)]

}


@(kill-evaluator rosette-eval)