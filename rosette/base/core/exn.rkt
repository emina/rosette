#lang racket/base

(provide raise-exn:fail:rosette:infeasible
         raise-exn:fail:rosette:assertion
         raise-exn:fail:rosette:assumption
         exn:fail:rosette?
         exn:fail:rosette:infeasible?
         exn:fail:rosette:assertion?
         exn:fail:rosette:assumption?)

(struct exn:fail:rosette exn:fail ())
(struct exn:fail:rosette:infeasible exn:fail:rosette ())
(struct exn:fail:rosette:assertion exn:fail:rosette ())
(struct exn:fail:rosette:assumption exn:fail:rosette ())

(define (raise-exn:fail:rosette:infeasible)
  (raise (exn:fail:rosette:infeasible
          "pc: infeasible path condition"
          (current-continuation-marks))))

(define (raise-exn:fail:rosette:assertion msg)
  (raise (exn:fail:rosette:assertion
          (string-append "assert: " msg)
          (current-continuation-marks))))

(define (raise-exn:fail:rosette:assumption msg)
  (raise (exn:fail:rosette:assertion
          (string-append "assume: " msg)
          (current-continuation-marks))))