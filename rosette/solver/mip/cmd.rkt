#lang racket

(require (only-in rosette sat unsat evaluate))
(require "ilp.rkt" "common.rkt" "mip-converter.rkt")
(provide encode decode)

(define (encode env asserts obj)
  (ilp-start)
  (if (equal? 'min (objective-type obj))
      (ilp-minimize (objective-expr obj) env)
      (ilp-maximize (objective-expr obj) env))
  
  (ilp-assert-init)
  (for ([a asserts]) (ilp-enc a env))
  (ilp-done))

(define (decode env convert)
  (define name2sym (converter-name2sym convert))
  (define mapping-info (converter-mapping-info convert))
  (define mip-sol-hash (make-hash))
  (for ([sym (hash-values name2sym)])
    (hash-set! mip-sol-hash sym 0))

  (read-line) ; Variable Name Solution Value
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
  (define smt-sol-hash (make-hash (hash->list mip-sol-hash)))

  (for ([pair (hash->list mapping-info)])
    (let ([org-sym (car pair)]
          [mip-syms-vals (cdr pair)]
          [org-val #f])
      (for ([mip-sym-val mip-syms-vals])
        (let* ([mip-sym (car mip-sym-val)]
               [smt-val (cdr mip-sym-val)]
               [mip-val (hash-ref mip-sol-hash mip-sym)])
          (cond
            [(= mip-val 0) (void)]
            [(= mip-val 1)
             (when org-val (raise (format "~a cannot be ~a and ~a at the same time." org-sym org-val smt-val)))
             (set! org-val smt-val)]
            [else (raise (format "~a should be either 0 or 1, but it is ~a." mip-sym mip-val))])
          (hash-remove! smt-sol-hash mip-sym)))
      (hash-set! smt-sol-hash org-sym org-val)
      ))

  (define smt-sol (sat (make-immutable-hash (hash->list smt-sol-hash))))

  (for ([mip-pair (converter-objs convert)]
        [smt-pair (converter-org-objs convert)])
    (let ([mip-o (objective-expr mip-pair)]
          [smt-o (objective-expr smt-pair)])
      (unless (= (evaluate mip-o mip-sol) (evaluate smt-o smt-sol))
        (raise (exn:fail (format "MIP objective is not equal to SMT objective.\nSMT: ~a = ~a\nMIP: ~a = ~a\n"
                                 (evaluate smt-o smt-sol) smt-o
                                 (evaluate mip-o mip-sol) mip-o))))))

  smt-sol
  )
