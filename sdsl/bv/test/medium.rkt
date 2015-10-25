#lang s-exp "../bv.rkt"

(require rackunit rackunit/text-ui "util.rkt" rosette/lib/util/roseunit)
(require "../examples/reference.rkt" "../examples/consts.rkt")

(define medium-tests
  (test-suite+ 
   "Hacker's Delight Problems 11-19"
   
   (parameterize ([current-bitwidth 32]
                  [verbose? #f])
     
     (test-fragment 
      (p11* x y) 
      #:implements p11
      #:library (bvlib [{bvnot bvand bvugt} 1]))
     
     (test-fragment 
      (p12* x y) 
      #:implements p12
      #:library (bvlib [{bvand bvnot bvule} 1]))
     
     (test-fragment 
      (p13* x) 
      #:implements p13
      #:library (bvlib [{const-max-shift bvor} 1] [{bvneg bvshr} 2]))
     
     (test-fragment 
      (p14* x y) 
      #:implements p14
      #:library (bvlib [{const bvand bvshr bvxor bvadd} 1]))
     
     (test-fragment 
      (p15* x y) 
      #:implements p15
      #:library (bvlib [{const bvor bvshr bvxor bvsub} 1]))
     
     (test-fragment 
      (p16* x y)
      #:implements p16
      #:library (bvlib [{bvneg bvge bvand} 1] [{bvxor} 2]))
     
     (test-fragment 
      (p17* x) 
      #:implements p17
      #:library (bvlib [{const bvand bvor bvadd bvsub} 1]))
     
     (test-fragment
      (p18* x) 
      #:implements p18
      #:library (bvlib [{const bvsub bvnot} 1] [{bvredor bvand} 2]))
     
     (test-fragment 
      (p19* x m k) 
      #:requires (lambda (x m k) (&& (<= 0 k) (<= k (current-bitwidth))))
      #:implements p19
      #:library (bvlib [{bvxor} 3] [{bvand bvshl bvshr} 1]))
     
     )))

(time (run-tests medium-tests))