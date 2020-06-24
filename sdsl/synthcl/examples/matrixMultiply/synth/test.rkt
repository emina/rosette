#lang racket

(require (only-in "host.rkt" synth_vector)
         (only-in "../../../lang/queries.rkt" expected? query-output-port)
         (only-in rosette sat? unsat? current-solver)
         rosette/solver/smt/boolector
         rackunit rackunit/text-ui rosette/lib/roseunit)

(when (boolector-available?)
  (current-solver (boolector)))

(define fast-tests
  (test-suite+ 
   "SynthCL: fast matrix-multiply synthesis tests"
   
   (parameterize ([query-output-port (open-output-nowhere)])
     
     (parameterize ([expected? unsat?])
       (synth_vector 1)
       (synth_vector 2)
       (synth_vector 3))
     
     (parameterize ([expected? sat?])
       (synth_vector 4)))))

(define slow-tests
  (test-suite+ 
   "SynthCL: slow matrix-multiply synthesis tests"
   
   (parameterize ([query-output-port (open-output-nowhere)])
     
     (parameterize ([expected? unsat?])
       (synth_vector 5)
       (synth_vector 6)
       (synth_vector 7))
     
     (parameterize ([expected? sat?])
       (synth_vector 8)))))

(module+ test
  (time (run-tests fast-tests))
  (time (run-tests slow-tests)))

(module+ fast
  (time (run-tests fast-tests)))
