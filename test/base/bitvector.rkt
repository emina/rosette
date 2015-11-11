#lang racket

(require rackunit rackunit/text-ui racket/generator
         rosette/lib/util/roseunit
         racket/fixnum 
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/bitvector
         (only-in rosette/base/form/define define-symbolic)
         "common.rkt" "exprs.rkt")

(define BV (bitvector 4))
(define-symbolic x y z BV)

 
(define e0 (list x y z))                    
(define e2 (test-exprs 2 (list (naive* @bvand) (naive* @bvor) (naive @bvnot)) e0))        
      
                
