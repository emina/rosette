#lang racket

(require
  (only-in "term.rkt" term? term-type)
  (only-in "bool.rkt" @boolean? @false? ! && =>)
  "vc.rkt")
