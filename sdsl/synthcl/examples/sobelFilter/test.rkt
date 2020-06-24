#lang racket

(require (only-in "host.rkt" verify_scalar verify_vectorized synth_vectorized)
         (only-in "reference.rkt" verify_0_1 verify_1_2 verify_2_3 verify_3_4 verify_4_5 synth_3)
         (only-in "../../lang/queries.rkt" expected? query-output-port)
         (only-in rosette sat? unsat? current-solver)
         rosette/solver/smt/boolector
         rackunit rackunit/text-ui rosette/lib/roseunit)

(when (boolector-available?)
  (current-solver (boolector)))

(define fast-tests
  (test-suite+ 
   "SynthCL: fast Sobel synthesis and verification tests"
   
   (parameterize ([query-output-port (open-output-nowhere)])
     
     (parameterize ([expected? unsat?])
       (verify_scalar)
       (verify_vectorized))
     
     (parameterize ([expected? sat?])
       (synth_vectorized)))))

(define slow-tests
  (test-suite+ 
   "SynthCL: slow Sobel synthesis and verification tests"
   
   (parameterize ([query-output-port (open-output-nowhere)]) 
     (parameterize ([expected? unsat?])
       (verify_0_1)
       (verify_1_2)
       (verify_2_3)
       (verify_3_4)
       (verify_4_5))
   
     (parameterize ([expected? sat?])
       (synth_3)))))

(module+ test
  (time (run-tests fast-tests))
  (time (run-tests slow-tests)))

(module+ fast
  (time (run-tests fast-tests)))
