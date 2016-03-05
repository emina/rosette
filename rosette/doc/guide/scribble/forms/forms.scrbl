#lang scribble/manual

@(require (for-label racket))


@title[#:tag "ch:syntactic-forms" #:style 'toc]{Syntactic Forms}

The core of the Rosette language (@racket[rosette/safe]) consists of two kinds of syntax forms: a set of basic  forms @deftech[#:key "lifted constructs"]{lifted} from Racket, and a set of forms for @seclink["ch:essentials"]{solver-aided programming}.  We use the term "lifted" to refer to parts of the Racket language that can be used with symbolic values and other solver-aided constructs.

@[table-of-contents]
@include-section["racket-forms.scrbl"]
@include-section["rosette-forms.scrbl"]

