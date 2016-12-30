#lang racket

(require "ilp.rkt" "common.rkt")
(provide encode)

(define (encode env asserts obj)
  (ilp-start)
  (if (equal? 'min (objective-type obj))
      (ilp-minimize (objective-expr obj) env)
      (ilp-maximize (objective-expr obj) env))
  
  (ilp-assert-init)
  (for ([a asserts]) (ilp-enc a env))
  (ilp-done))