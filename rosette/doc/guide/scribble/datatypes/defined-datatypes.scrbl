#lang scribble/manual

@(require (for-label 
           rosette/base/form/define rosette/query/query rosette/query/eval  
           rosette/base/core/term (only-in rosette/base/core/safe assert) 
           racket racket/generic)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")

@(define rosette-eval (rosette-evaluator))
@(define prop-facilities (select '(make-struct-type-property struct-type-property? struct-type-property-accessor-procedure?)))
@(define props (select '(prop:arity-string prop:blame prop:chaperone-contract prop:chaperone-unsafe-undefined prop:checked-procedure prop:contract prop:contracted prop:custom-print-quotable prop:custom-write prop:dict prop:dict/contract prop:equal+hash prop:evt prop:exn:missing-module prop:exn:srclocs prop:flat-contract prop:impersonator-of prop:input-port prop:legacy-match-expander prop:liberal-define-context prop:match-expander prop:output-port prop:place-location prop:procedure prop:provide-pre-transformer prop:provide-transformer prop:rename-transformer prop:require-transformer prop:sequence prop:serializable prop:set!-transformer prop:stream prop:struct-auto-info prop:struct-info)))
@(define generics-facilities (select '(define-generics raise-support-error exn:fail:support define/generic generic-instance/c impersonate-generics chaperone-generics redirect-generics )))
@(define generics (select '(gen:custom-write gen:dict gen:equal+hash gen:set gen:stream)))


@title[#:tag "ch:programmer-defined-datatypes" #:style 'toc]{Structures}

In addition to @tech[#:key "lifted constructs"]{lifting} many
@seclink["ch:built-in-datatypes"]{built-in datatypes}
to work with symbolic values, Rosette also lifts Racket's
@racketlink[struct]{structures}.
As in Racket, a structure is an instance of a @deftech{structure type}---a
record datatype with zero or more fields. 
Structure types are defined  using the @racket[struct] syntax. Defining a
structure type in this way also defines the necessary procedures for
creating instances of that type and for accessing their fields. 

@(rosette-eval '(require (only-in racket [struct racket/struct])))
@(rosette-eval '(racket/struct point (x y) #:transparent))
@(racketblock
(struct point (x y) #:transparent)         (code:comment "immutable transparent type")
(struct pt (x y))                          (code:comment "opaque immutable type")
(struct pnt (x y) #:mutable #:transparent) (code:comment "mutable transparent type")
)

Rosette structures can be concrete or symbolic.  Their semantics matches that of Racket, 
with one important exception:  immutable transparent structures are treated as values 
rather than references.  This @seclink["sec:equality"]{means} that two such structures are 
@racket[eq?] if they belong to the same type and their corresponding field values are @racket[eq?].
Structures of opaque or mutable types are treated as references.  Two such structures are
@racket[eq?] only if the point to the same instance of the same type.

@interaction[#:eval rosette-eval
(eval:alts (code:line (eq? (point 1 2) (point 1 2))  (code:comment "point structures are values")) #t)
(eval:alts (code:line (eq? (pt 1 2) (pt 1 2))        (code:comment "pt structures are references")) #f)
(eval:alts (code:line (eq? (pnt 1 2) (pnt 1 2))      (code:comment "pnt structures are references")) #f)]

Like @tech[#:key "unsolvable type"]{unsolvable built-in datatypes}, 
symbolic structures cannot be created using @racket[define-symbolic].  Instead, 
they are created implicitly, by, for example, using an @racket[if] expression 
together with a symbolic value.


@interaction[#:eval rosette-eval
(define-symbolic b boolean?)
(eval:alts (code:line (define p (if b (point 1 2) (point 3 4))) (code:comment "p holds a symbolic structure"))
           (define p (if b (cons 1 2) (cons 3 4))))
(eval:alts (point-x p) (car p))
(eval:alts (point-y p) (cdr p))
(eval:alts (define sol (solve (assert (= (point-x p) 3)))) (define sol (solve (assert (= (car p) 3)))))
(eval:alts (evaluate p sol) (point 3 4))]

As well as lifting the @racket[struct] syntax, Rosette also lifts the following structure 
properties, generic interfaces, and facilities for defining new properties and interfaces:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Defining Properties} @elem{@prop-facilities})
      (list @elem{Lifted Properties} @elem{@props})
      (list @elem{Defining Generics} @elem{@generics-facilities})
      (list @elem{Lifted Generics} @elem{@generics} ))]

Lifted generics work as expected with symbolic values:
@(rosette-eval '(racket/struct circle (radius) #:transparent))
@(racketblock
(define-generics viewable (view viewable))

(struct square (side) 
  #:methods gen:viewable
  [(define (view self) (square-side self))])

(struct circle (radius)
  #:transparent
  #:methods gen:viewable
  [(define (view self) (circle-radius self))]))
@interaction[#:eval rosette-eval
(define-symbolic b boolean?)
(eval:alts (code:line (define p (if b (square 2) (circle 3))) (code:comment "p holds a symbolic structure"))
           (define p (if b 2 3)))
(eval:alts (view p) p)
(eval:alts (define sol (solve (assert (= (view p) 3)))) (define sol (solve (assert (= p 3)))))
(eval:alts (evaluate p sol) (circle 3))]

@(kill-evaluator rosette-eval)