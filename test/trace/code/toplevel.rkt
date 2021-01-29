#lang rosette

(define-symbolic x integer?)

(verify (1))

(synthesize #:forall x
            #:guarantee (2))

(solve (3))
