#lang scribble/manual

@(require (for-label 
           rosette/base/define rosette/query/tools rosette/query/eval  
           rosette/base/term rosette/base/primitive
           (only-in rosette/base/safe assert) 
           racket racket/generic)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")

@(define rosette-eval (rosette-evaluator))
@(define prop-facilities (select '(make-struct-type-property struct-type-property? struct-type-property-accessor-procedure?)))
@(define props (select '(prop:arity-string prop:blame prop:chaperone-contract prop:chaperone-unsafe-undefined prop:checked-procedure prop:contract prop:contracted prop:custom-print-quotable prop:custom-write prop:dict prop:dict/contract prop:equal+hash prop:evt prop:exn:missing-module prop:exn:srclocs prop:flat-contract prop:impersonator-of prop:input-port prop:legacy-match-expander prop:liberal-define-context prop:match-expander prop:output-port prop:place-location prop:procedure prop:provide-pre-transformer prop:provide-transformer prop:rename-transformer prop:require-transformer prop:sequence prop:serializable prop:set!-transformer prop:stream prop:struct-auto-info prop:struct-info)))
@(define generics-facilities (select '(define-generics raise-support-error exn:fail:support define/generic generic-instance/c impersonate-generics chaperone-generics redirect-generics )))
@(define generics (select '(gen:custom-write gen:dict gen:equal+hash gen:set gen:stream)))

@title[#:tag "sec:struct"]{Structures}

A @deftech{structure type} is a record datatype that includes zero or more fields. 
A @deftech{structure} is an instance of a structure type; it is a first-class value 
that maps each field of its type to a value.  Structure types are defined  
using Racket's @racket[struct] syntax. Defining a structure type in this way also 
defines the necessary procedures for creating instances of that type and for accessing 
their fields.


Rosette structures can be concrete or symbolic.  Their semantics matches that of Racket, 
with one important exception:  immutable transparent structures are treated as values 
rather than references.  This @seclink["sec:equality"]{means} that two such structures are 
@racket[eq?] if they belong to the same type and their corresponding field values are @racket[eq?].

@examples[#:eval rosette-eval
(eval:alts (code:line (struct point (x y) #:transparent) (code:comment "immutable transparent type")) (void))
(eval:alts (code:line (eq? (point 1 2) (point 1 2)) (code:comment "point structures are values")) #t)
(eval:alts (code:line (struct pt (x y)) (code:comment "opaque immutable type")) (void))
(eval:alts (code:line (eq? (pt 1 2) (pt 1 2)) (code:comment "pt structures are references")) #f)
(eval:alts (code:line (struct pnt (x y) #:mutable #:transparent) (code:comment "mutable transparent type")) (void))
(eval:alts (code:line (eq? (pnt 1 2) (pnt 1 2)) (code:comment "pnt structures are references")) #f)]

Like @tech[#:key "composite datatype"]{composite built-in datatypes}, 
symbolic structures cannot be created using @racket[define-symbolic].  Instead, 
they are created implicitly, by, for example, using an @racket[if] expression 
together with a symbolic value.

@(rosette-eval '(require (only-in racket [struct racket/struct])))
@examples[#:eval rosette-eval
(eval:alts (code:line (struct point (x y) #:transparent) (code:comment "immutable transparent type")) 
           (racket/struct point (x y) #:transparent))
(define-symbolic b boolean?)
(eval:alts (code:line (define p (if b (point 1 2) (point 3 4))) (code:comment "p holds a symbolic structure"))
           (define p (if b (cons 1 2) (cons 3 4))))
(eval:alts (point-x p) (car p))
(eval:alts (point-y p) (cdr p))
(eval:alts (define env (solve (assert (= (point-x p) 3)))) (define env (solve (assert (= (car p) 3)))))
(eval:alts (evaluate p env) (point 3 4))]

@section{Structure Type Properties and Generic Interfaces}

In addition to lifting the @racket[struct] syntax, Rosette also lifts the following structure 
properties, generic interfaces, and facilities for defining new properties and interfaces:
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @elem{Defining Properties} @elem{@prop-facilities})
      (list @elem{Lifted Properties} @elem{@props})
      (list @elem{Defining Generics} @elem{@generics-facilities})
      (list @elem{Lifted Generics} @elem{@generics} ))]

@(kill-evaluator rosette-eval)