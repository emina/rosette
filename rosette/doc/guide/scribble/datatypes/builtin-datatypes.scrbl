#lang scribble/manual


@(require (for-label racket)
          (for-label rosette/base/form/define rosette/base/core/type))

@title[#:tag "ch:built-in-datatypes" #:style 'toc]{Built-In Datatypes}

The @seclink["ch:syntactic-forms"]{previous chapter} describes the  
Racket syntax forms that are @tech[#:key "lifted constructs"]{lifted} by Rosette to
work on symbolic values. 
This chapter describes the lifted datatypes and their corresponding operations. Most 
lifted operations retain their Racket semantics, with the exception of  
equality predicates (Section @seclink["sec:equality"]{4.1}) and
numeric operations (Section @seclink["sec:bools+ints+reals"]{4.2}).

Rosette distinguishes between two kinds of built-in datatypes:
@deftech[#:key "solvable type"]{solvable} and @deftech[#:key "unsolvable type"]{unsolvable}.
Solvable types are (efficiently) supported by SMT solvers, and they include booleans,
integers, reals, bitvectors, and uninterpreted functions.  All other built-in types are
unsolvable---that is, not as well supported by SMT solvers.   

Every lifted type is equipped with a predicate (e.g., @racket[boolean?]) that
recognizes values of that type.  Solvable types are themselves recognized by
the @racket[solvable?] predicate.  Lifted types include both concrete Racket
values and symbolic Rosette values, but only solvable types include
@tech[#:key "symbolic constant"]{symbolic constants},
as introduced by @seclink["sec:symbolic-constants"]{@code{define-symbolic[*]}}.

@(table-of-contents)
@include-section["equality.scrbl"]
@include-section["bools+ints+reals.scrbl"]
@include-section["bitvectors.scrbl"]
@include-section["uninterpreted.scrbl"]
@include-section["procedures.scrbl"]
@include-section["pairs.scrbl"]
@include-section["vectors.scrbl"]
@include-section["boxes.scrbl"]
@include-section["solvers+solutions.scrbl"]







