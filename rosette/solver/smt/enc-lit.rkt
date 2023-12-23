#lang racket/base

(provide current-enc-lit
         enc-real
         enc-integer)

(require racket/match
         racket/format
         (prefix-in $ "smtlib2.rkt")
         (only-in "../../base/core/bitvector.rkt" bv bitvector-size))

(define (enc-lit v)
  (match v
    [#t $true]
    [#f $false]
    [(? integer?) (enc-integer v)]
    [(? real?) (enc-real v)]
    [(bv lit t) ($bv lit (bitvector-size t))]
    [_ (error 'enc "expected a boolean?, integer?, real?, or bitvector?, given ~a" v)]))

(define current-enc-lit
  (make-parameter enc-lit))

(define-syntax-rule (enc-real v)
  (if (exact? v) ($/ (numerator v) (denominator v)) (string->symbol (~r v))))

(define-syntax-rule (enc-integer v)
  (let ([v* (inexact->exact v)])
    (if (< v* 0) ($- (abs v*)) v*)))
