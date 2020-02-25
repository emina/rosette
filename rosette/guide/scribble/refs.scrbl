#lang scribble/manual

@(require scriblib/autobib scribble/core (only-in racket match))
@(provide (all-defined-out))

@(define-cite ~cite citet generate-bibliography #:style number-style)

@(abbreviate-given-names #t)

@(define rosette:onward13
   (make-bib
     #:title @hyperlink["http://homes.cs.washington.edu/~emina/pubs/rosette.onward13.pdf"]{Growing Solver-Aided Languages with Rosette}
     #:author (authors "Emina Torlak" "Rastislav Bodik")
     #:date 2013
     #:location "New Ideas, New Paradigms, and Reflections on Programming and Software (Onward!)"))

@(define rosette:pldi14
   (make-bib
    #:title @hyperlink["http://homes.cs.washington.edu/~emina/pubs/rosette.pldi14.pdf"]{A Lightweight Symbolic Virtual Machine for Solver-Aided Host Languages}
    #:author (authors "Emina Torlak" "Rastislav Bodik")
    #:date 2014
    #:location "Programming Language Design and Implementation (PLDI)"))

@(define sympro:oopsla18
   (make-bib
    #:title @hyperlink["https://unsat.cs.washington.edu/papers/bornholt-sympro.pdf"]{Finding Code That Explodes Under Symbolic Evaluation}
    #:author (authors "James Bornholt" "Emina Torlak")
    #:date 2018
    #:location "Object Oriented Programming, Systems, Languages, and Applications (OOPSLA)"))