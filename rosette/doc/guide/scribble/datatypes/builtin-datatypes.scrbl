#lang scribble/manual




@title[#:tag "ch:built-in-datatypes" #:style 'toc]{Built-In Datatypes}

The @seclink["ch:syntactic-forms"]{previous chapter} describes the  
Racket syntax forms that are @tech[#:key "lifted constructs"]{lifted} by Rosette to
work on symbolic values. 
This chapter describes the lifted datatypes and their corresponding operations. Most 
lifted operations retain their Racket semantics, with the exception of  
numeric functions (Section @seclink["sec:primitives"]{4.1}) and 
equality predicates (Section @seclink["sec:equality"]{4.2}).

@(table-of-contents)
@include-section["primitives.scrbl"]
@include-section["equality.scrbl"]
@include-section["pairs.scrbl"]
@include-section["vectors.scrbl"]
@include-section["boxes.scrbl"]
@include-section["procedures.scrbl"]
@include-section["solvers+solutions.scrbl"]







