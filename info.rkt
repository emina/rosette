#lang info

(define collection 'multi)

(define deps '("no-vert-bar-lang"
               "r6rs-lib"
               "rackunit-lib"
               "slideshow-lib"
               "base"))
(define build-deps '("pict-doc"
                     "scribble-lib"
                     "racket-doc"))

(define pkg-desc "Rosette solver-aided host language")
(define version "2")
