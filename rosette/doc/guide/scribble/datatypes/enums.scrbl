#lang scribble/manual

@(require (for-label 
           rosette/base/define rosette/solver/solution rosette/query/tools rosette/query/eval 
           rosette/base/term rosette/base/primitive rosette/base/enum
           (only-in rosette/base/safe assert) 
           racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")


@(define rosette-eval (rosette-evaluator))

@declare-exporting[rosette/base/enum
                   #:use-sources 
                   (rosette/base/enum)]


@title[#:tag "sec:enum"]{Enumerations}

An @deftech{enumerated datatype} is a type consisting of an ordered set of labeled concrete 
elements. Enumerated types also contain symbolic values.  A symbolic value of an enumerated 
type evaluates to one of its concrete elements under a @racket[solution?] returned by a 
solver-aided query.  Like @seclink["sec:primitives"]{primitive datatypes}, enumerated types 
include symbolic constants, which can be created using @racket[define-symbolic] or @racket[define-symbolic*].

@defform[(define-enum id labels)#:contracts
         [(labels list?)]]{
  Creates an enumerated type @var[id?] consisting of elements that are 
  labeled with the given list of @racket[labels].  The label values must be
  distinct according to @racket[equal?], and they must be immutable. Elements 
  of the resulting type are ordered according to the @racket[labels] list, so that 
  the i@superscript{th} element has the i@superscript{th} label.
  
  Elements of @var[id?] are recognized by the predicate @var[id?], and 
  they are ordered by the predicate @var[id<?].  The identifer @racket[id] is a bound to a 
  procedure that takes as input a label and returns the corresponding enum element.
  @examples[#:eval rosette-eval
  (define-enum suit '(club diamond heart spade))
  (suit 'club) 
  (suit? (suit 'club))
  (suit<? (suit 'diamond) (suit 'heart))
  (define-symbolic s suit?)
  (define env (solve (assert (suit<? s (suit 'diamond)))))
  (evaluate s env)  
  (suit "club")
  ]
}

@section{Generic Operations on Enumerated Datatypes}

Rosette provides the following generic procedures for operating on enum types and 
elements:

@defproc[(enum? [t any/c]) boolean?]{
Returns true iff @racket[t] is a concrete predicate that recognizes 
memebers of an enumerated datatype.  
@examples[#:eval rosette-eval
  (define-enum suit '(club diamond heart spade))
  (enum? suit?)
  (enum? number?)
  (define-symbolic b boolean?)
  (enum? (if b suit? number?))
  ]
}

@defproc[(label [element any/c]) any/c]{
Returns the label of the given (concrete or symbolic) enum element, or throws an error 
if the given value is not an element of an enumerated datatype.
@examples[#:eval rosette-eval
  (define-enum rgb '(red green blue))
  (label (rgb 'green))
  (define-symbolic c rgb?)
  (label c) 
  (label "green")
  ]
}

@defproc[(ordinal [element any/c]) natural/c]{
Returns the ordinal of the given (concrete or symbolic) enum element, or throws an error 
if the given value is not an element of an enumerated datatype.
@examples[#:eval rosette-eval
  (define-enum rgb '(red green blue))
  (ordinal (rgb 'green))
  (define-symbolic c rgb?)
  (ordinal c) 
  (ordinal "green")
  ]
}



@(kill-evaluator rosette-eval)