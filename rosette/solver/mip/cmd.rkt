#lang racket

(require (only-in rosette sat unsat evaluate))
(require "enc.rkt" "common.rkt")
(provide encode decode)

; Given a list of asserts and a list of objectives,
; the encode procedure prints an MIP encoding of the given assertions
; to current-output-port. 
(define (encode asserts obj)
  (mip-start)
  (if (equal? 'min (objective-type obj))
      (mip-minimize (objective-expr obj))
      (mip-maximize (objective-expr obj)))
  
  (mip-assert-start)
  (for ([a asserts]) (mip-enc a))
  (mip-done))

; Given a mapping of name to symbolic variable,
; Decode procedure reads the solution from current-input-port
; and converts it into a Rosette solution.  
(define (decode name2sym)
  (define mip-sol-hash (make-hash))

  ;; Set all variables to 0.
  (for ([sym (hash-values name2sym)])
    (hash-set! mip-sol-hash sym 0))

  ;; Loop to read all variables.
  (define (loop)
    (define line (read-line))
    (unless (or (regexp-match #rx"All other variables" line)
                (regexp-match #rx"CPLEX>" line))
      (define toks (string-split line))
      (define sym (hash-ref name2sym (first toks)))
      (define val (string->number (second toks)))
      (when (integer? val) (set! val (inexact->exact val)))
      (hash-set! mip-sol-hash sym val)
      (loop)))
  (loop)
  
  (define mip-sol (sat (make-immutable-hash (hash->list mip-sol-hash))))
  mip-sol
  )
