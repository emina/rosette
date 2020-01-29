#lang scribble/manual

@(require (for-label racket))

@title[#:tag "ch:libraries" #:style 'toc]{Libraries}

Chapters @seclink["ch:getting-started"]{1}-@seclink["ch:programmer-defined-datatypes"]{5} introduce the basic constructs and datatypes for programming in Rosette. This chapter describes the parts of the core Racket libraries (e.g., I/O procedures) that are exported by @racket[rosette/safe], as well as Rosette libraries that provide additional facilities for solver-aided development.

@[table-of-contents]


@include-section["racket-libs.scrbl"]
@include-section["rosette-libs.scrbl"]
@include-section["utility-libs.scrbl"]
