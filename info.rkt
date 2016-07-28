#lang info

(define collection 'multi)

(define deps '("r6rs-lib"
               "rackunit-lib"
               "slideshow-lib"
               "base"))

(define build-deps '("pict-doc"
                     "scribble-lib"
                    "racket-doc"))

(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))

(define pkg-desc "Rosette solver-aided host language")
(define version "2.2")
