#lang scribble/manual

@(require (for-label racket))


@title[#:tag "ch:programmer-defined-datatypes" #:style 'toc]{Programmer-Defined Datatypes}

@seclink["ch:built-in-datatypes"]{Chapter 4} presents the built-in Racket datatypes that 
are lifted by Rosette to work in the presence of symbolic values.  This chapter introduces two mechanisms 
for creating new programmer-defined datatypes:  @seclink["sec:struct"]{structures} and 
@seclink["sec:enum"]{enumerations}.  Rosette structures lift Racket structures to work 
with symbolic values.  Enumerations are similar to Java's enums, and they 
can also be used with solver-aided facilities.  

@[table-of-contents]
@include-section["structs.scrbl"]
@include-section["enums.scrbl"]
