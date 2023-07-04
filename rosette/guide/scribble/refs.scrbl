#lang scribble/manual

@(require scriblib/autobib scribble/core (only-in racket match))
@(provide (all-defined-out))

@(define-cite ~cite citet generate-bibliography #:style number-style)

@(abbreviate-given-names #t)

@(define rosette:onward13
   (make-bib
     #:title @hyperlink["http://dl.acm.org/citation.cfm?id=2509586"]{Growing Solver-Aided Languages with Rosette}
     #:author (authors "Emina Torlak" "Rastislav Bodik")
     #:date 2013
     #:location "New Ideas, New Paradigms, and Reflections on Programming and Software (Onward!)"))

@(define rosette:pldi14
   (make-bib
    #:title @hyperlink["http://dl.acm.org/citation.cfm?id=2594340"]{A Lightweight Symbolic Virtual Machine for Solver-Aided Host Languages}
    #:author (authors "Emina Torlak" "Rastislav Bodik")
    #:date 2014
    #:location "Programming Language Design and Implementation (PLDI)"))

@(define sympro:oopsla18
   (make-bib
    #:title @hyperlink["https://dl.acm.org/citation.cfm?id=3276519"]{Finding Code That Explodes Under Symbolic Evaluation}
    #:author (authors "James Bornholt" "Emina Torlak")
    #:date 2018
    #:location "Object Oriented Programming, Systems, Languages, and Applications (OOPSLA)"))

@(define rosette:popl22
   (make-bib
    #:title @hyperlink["https://doi.org/10.1145/3498709"]{A Formal Foundation for Symbolic Evaluation with Merging}
    #:author (authors "Sorawee Porncharoenwase" "Luke Nelson" "Xi Wang" "Emina Torlak")
    #:date 2022
    #:location "Principles of Programming Languages (POPL)"))