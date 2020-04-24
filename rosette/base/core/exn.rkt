#lang racket/base

(provide raise-exn:fail:rosette:infeasible
         raise-exn:fail:rosette:assertion
         exn:fail:rosette?
         exn:fail:rosette:infeasible?
         exn:fail:rosette:assertion?)

(struct exn:fail:rosette exn:fail ())
(struct exn:fail:rosette:infeasible exn:fail:rosette ())
(struct exn:fail:rosette:assertion exn:fail:rosette ())

(define-syntax-rule (raise-exn:fail:rosette:infeasible)
  (raise (exn:fail:rosette:infeasible
          "pc: infeasible path condition"
          (current-continuation-marks))))

(define-syntax-rule (raise-exn:fail:rosette:assertion msg)
  (raise (exn:fail:rosette:assertion
          (string-append "assert: " msg)
          (current-continuation-marks))))
