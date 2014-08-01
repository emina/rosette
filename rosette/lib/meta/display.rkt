#lang racket

(require "generate.rkt"
         (only-in rosette/query/state current-solution))

(provide print-forms print-expressions)

(define (print-forms [sol (current-solution)] #:filter [print? any/c])
  (for ([synth (sort (generate-forms sol #:filter print?) stx<? #:key car)])
    (printf "\n~a\n~a\n" (car synth) (pretty-format (syntax->datum (cdr synth))))))

(define (print-expressions [sol (current-solution)] #:filter [print? any/c])
  (for ([synth (sort (generate-expressions sol #:filter print?) stx<? #:key car)])
    (printf "\n~a\n~a\n" (car synth) (pretty-format (syntax->datum (cdr synth))))))

(define (source->string stx)
  (let ([src (syntax-source stx)])
    (cond [(false? src) ""]
          [(string? src) src]
          [(path? src) (path->string src)]
          [else (~.a src)])))

(define (stx<? s0 s1)
  (let* ([src0 (source->string s0)]
         [src1 (source->string s1)])
    (or (string<? src0 src1)
        (and (equal? src0 src1)
             (let ([ln0 (or (syntax-line s0) -1)]
                   [ln1 (or (syntax-line s1) -1)])
               (or (< ln0 ln1)
                   (and (= ln0 ln1)
                        (let ([cl0 (or (syntax-column s0) -1)]
                              [cl1 (or (syntax-column s1) -1)])
                          (< cl0 cl1)))))))))
