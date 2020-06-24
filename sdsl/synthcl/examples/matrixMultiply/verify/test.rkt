#lang racket

(require (only-in "host.rkt" verify_scalar verify_vector verify_vector_opt)
         (prefix-in buggy_ (only-in "host-buggy.rkt" verify_scalar verify_vector verify_vector_opt))
         (only-in "../../../lang/queries.rkt" expected? query-output-port)
         (only-in rosette sat? unsat? current-solver)
         rosette/solver/smt/boolector
         rackunit rackunit/text-ui rosette/lib/roseunit)

(when (boolector-available?)
  (current-solver (boolector)))

(define fast-tests
  (test-suite+ 
   "SynthCL: fast matrix-multiply verification tests"
   
   (parameterize ([query-output-port (open-output-nowhere)])
     
     (parameterize ([expected? unsat?])
       (verify_scalar 1 5)
       (verify_vector 4 9)
       (verify_vector_opt 4 9))
     
     (parameterize ([expected? sat?])
       (buggy_verify_scalar 1 5)
       (buggy_verify_vector 4 9)
       (buggy_verify_vector_opt 4 9)))))

(define slow-tests
  (test-suite+ 
   "SynthCL: slow matrix-multiply verification tests"
   
   (parameterize ([query-output-port (open-output-nowhere)])
     (parameterize ([expected? unsat?])
       (verify_scalar 5 9)
       (verify_vector 12 17)
       (verify_vector_opt 12 17)))))

(module+ test
  (time (run-tests fast-tests))
  (time (run-tests slow-tests)))

(module+ fast
  (time (run-tests fast-tests)))
