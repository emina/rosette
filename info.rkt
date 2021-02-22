#lang info

(define collection 'multi)

(define deps '("custom-load"
               "sandbox-lib"
               "scribble-lib"
               ("racket" #:version "8.0")
               "r6rs-lib"
               "rfc6455"
               "net-lib"
               "web-server-lib"
               "rackunit-lib"
               "slideshow-lib"
               "gui-lib"
               "base"))

(define build-deps '("rackunit-doc"
                     "draw-lib"
                     "errortrace-lib"
                     "pict-lib"
                     "pict-doc"
                     "scribble-lib"
                     "racket-doc"
                     "gui-doc"
                     "errortrace-doc"))

(define test-omit-paths (if (getenv "PLT_PKG_BUILD_SERVICE") 'all '()))

(define pkg-desc "Rosette solver-aided host language")
(define version "4.0")
