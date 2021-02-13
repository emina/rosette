#lang scribble/manual

@(require (for-label racket))


@title[#:tag "ch:symbolic-reflection" #:style 'toc]{Symbolic Reflection}

This chapter describes @deftech{symbolic reflection}, a  
mechanism for manipulating the representation of symbolic values 
(Section @seclink["sec:value-reflection"]{7.1}) and 
the state of the symbolic evaluation from within a Rosette program 
(Section @seclink["sec:state-reflection"]{7.2}).
Symbolic reflection has three main applications:  (1) @tech[#:key "lifted constructs"]{lifting} 
additional Racket constructs to work with symbolic values; 
(2) guiding Rosette's symbolic virtual machine to achieve 
better performance; and (3) implementing advanced solver-aided functionality.


@[table-of-contents]
@include-section["value-reflection.scrbl"]
@include-section["state-reflection.scrbl"]