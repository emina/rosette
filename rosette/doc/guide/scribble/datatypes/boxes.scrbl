#lang scribble/manual

@(require (for-label 
           rosette/base/define racket)
          scribble/core scribble/html-properties scribble/eval racket/sandbox
          "../util/lifted.rkt")

@(define box-ops (select '(box? box box-immutable unbox set-box! box-cas!)))

@title[#:tag "sec:box"]{Boxes}

A box is a single (im)mutable storage cell, which behaves like a one-element (im)mutable @seclink["sec:vec"]{vector}.
Lifted box operations are shown below.
@tabular[#:style (style #f (list (attributes '((id . "lifted")(class . "boxed")))))
(list (list @box-ops))]


