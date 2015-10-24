#lang racket

(require (only-in "host.rkt" verify_scalar verify_vector)
         (only-in "../../../lang/queries.rkt" expected? query-output-port)
         (only-in rosette sat? unsat?)
         rackunit rackunit/text-ui)

(define fast-tests
  (test-suite 
   "SynthCL: fast Walsh transform verification tests"
   #:before (lambda () (printf "SynthCL: fast Walsh transform synthesis tests.\n"))
   
   (parameterize ([query-output-port (open-output-nowhere)])
     
     (parameterize ([expected? unsat?])
       (verify_scalar)
       (verify_vector)))))

(module+ test
  (time (run-tests fast-tests)))