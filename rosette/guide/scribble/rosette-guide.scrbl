#lang scribble/manual

@(require (for-label racket) (only-in racket match)
           racket/runtime-path (only-in racket build-path)
           scribble/core scribble/html-properties)

@(define-runtime-path guide.css "util/guide.css")
@(define guide-style
     (make-style "GuideStyle"
                 (list (make-css-addition guide.css))))

@title[#:style guide-style]{The Rosette Guide}
@author{Emina Torlak}

This document is intended both as an introduction to solver-aided programming with Rosette,
and as a reference manual for the Rosette language. It assumes @hyperlink["http://racket-lang.org"]{Racket}
programming experience, so if you are unfamiliar with Racket, 
you may want to start by reading @hyperlink["http://docs.racket-lang.org/guide/"]{The Racket Guide}. 

Chapters @seclink["ch:getting-started"]{1} and @seclink["ch:essentials"]{2} introduce the Rosette system and illustrate the key concepts of solver-aided programming. 
Chapters @seclink["ch:syntactic-forms"]{3}-@seclink["ch:libraries"]{6} define the core Rosette language 
(@seclink["sec:langs"]{@racket[rosette/safe]}) and describe its main libraries.  Chapter @seclink["ch:symbolic-reflection"]{7} and 
@seclink["ch:unsafe"]{8} describe the advanced features of the full language (@seclink["sec:langs"]{@racket[rosette]}).  If you are new to Rosette, consider starting with the core language.  The full language is richer than the core, but it can also be @seclink["sec:langs"]{harder to use}.  

@defmodulelang*[(rosette/safe rosette)]

@(table-of-contents)

@include-section["welcome/welcome.scrbl"]
@include-section["essentials/essentials.scrbl"]
@include-section["forms/forms.scrbl"]
@include-section["datatypes/builtin-datatypes.scrbl"]
@include-section["datatypes/defined-datatypes.scrbl"]
@include-section["libs/libraries.scrbl"]
@include-section["reflection/symbolic-reflection.scrbl"]
@include-section["unsafe/unsafe.scrbl"]
@include-section["performance/performance.scrbl"]
@include-section["error-tracing/error-tracing.scrbl"]

@(require (only-in "refs.scrbl" generate-bibliography))
@(define bib @(generate-bibliography #:tag "refs" #:sec-title "References"))
@(match bib
     [(part tag-prefix tags title-content _ to-collect (list (table _ rest)) parts)
      (part tag-prefix tags title-content (style #f '(toc)) 
            to-collect 
            (list (table (style #f (list (attributes '((class . "bib"))))) rest))
            parts)])

@index-section[]
