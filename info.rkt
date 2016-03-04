#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("scribble-lib"
                     "racket-doc"))

(define pkg-desc "Rosette Solver")
(define compile-omit-paths '("sdsl"
                             "test"
                             "bin"))

;; XXX Move doc/ to rosette-doc/ when docs are written to build docs.