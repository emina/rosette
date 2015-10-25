#lang s-exp "../bv.rkt"

(require rackunit rackunit/text-ui "util.rkt" rosette/lib/util/roseunit)
(require "../examples/reference.rkt" "../examples/consts.rkt")

(define easy-tests
  (test-suite+ 
   "Hacker's Delight Problems 1-10"
   
   (parameterize ([current-bitwidth 32]
                  [verbose? #f])
     (test-fragment 
      (p1* x) 
      #:implements p1
      #:library (bvlib [{const bvand bvsub} 1]))
     
     (test-fragment 
      (p2* x) 
      #:implements p2
      #:library (bvlib [{const bvand bvadd} 1]))
     
     (test-fragment 
      (p3* x) 
      #:implements p3
      #:library (bvlib [{bvand bvneg} 1]))
     
     (test-fragment 
      (p4* x) 
      #:implements p4
      #:library (bvlib [{const bvsub bvxor} 1]))
     
     (test-fragment 
      (p5* x) 
      #:implements p5 
      #:library (bvlib [{const bvsub bvor} 1]))
     
     (test-fragment 
      (p6* x) 
      #:implements p6
      #:library (bvlib [{const bvor bvadd} 1]))
     
     (test-fragment 
      (p7* x) 
      #:implements p7
      #:library (bvlib [{bvand bvadd bvnot const} 1]))
     
     (test-fragment 
      (p8* x) 
      #:implements p8
      #:library (bvlib [{const bvsub bvand bvnot} 1]))
     
     (test-fragment 
      (p9* x) 
      #:implements p9
      #:library (bvlib [{const-max-shift bvsub bvxor bvshr} 1]))
     
     (test-fragment 
      (p10* x y) 
      #:implements p10
      #:library (bvlib [{bvand bvxor bvule} 1])))))

(time (run-tests easy-tests))

