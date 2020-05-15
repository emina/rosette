#lang rosette

(define-symbolic x integer?)

(verify #:assume (1)
        #:guarantee (2))

(synthesize #:forall x
            #:assume (1)
            #:guarantee (2))

(solve (1))
