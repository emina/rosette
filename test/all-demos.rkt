#lang racket

(require racket/sandbox)
(require "stats/stats.rkt")
(require rosette/base/util/log rosette/query/state rosette/solver/solution)
(require (only-in rosette  oracle current-oracle clear-asserts unsafe-clear-terms!))
(current-log-handler (log-handler #:info none/c))

(define (run-file file)
  (parameterize ([current-namespace (make-base-namespace)]
                 [current-solution (empty-solution)]
                 [current-oracle (oracle)]
                 [current-output-port (open-output-nowhere)])
    (fprintf (current-error-port) "Executing ~a\n" file)
    (eval `(require ,file))
    (clear-asserts)
    (unsafe-clear-terms!)))

(define (websynth)
  (run-file "../sdsl/websynth/benchmarks/itunes100_2.rkt")
  (run-file "../sdsl/websynth/benchmarks/imdb250_2.rkt")
  (run-file "../sdsl/websynth/benchmarks/alanon_arkansas_2.rkt"))

(define (hackers-delight)
  (for ([i (in-range 1 19)])  
    (run-file (format "../sdsl/bv/examples/p~a.rkt" i))))

(run/time/log (websynth) "stats/all-demos.txt")
(run/time/log (hackers-delight) "stats/all-demos.txt")