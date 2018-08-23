#lang rosette

(require "../bv.rkt")
(require rackunit rackunit/text-ui "util.rkt" rosette/lib/roseunit)
(require "../examples/reference.rkt")

(define medium-tests
  (test-suite+ 
   "Hacker's Delight Problems 11-19"
   (parameterize ([BV (bitvector 32)]
                  [verbose? #f])
     
     ; Test if nlz(x) < nlz(y) where nlz is number of leading zeroes. < 1 sec.
     (test-fragment (p11* x y) 
                    #:implements p11
                    #:library (bvlib [{bvnot bvand bvugt} 1]))
     
     ; Test if nlz(x) <= nlz(y) where nlz is number of leading zeroes. < 1 sec.
     (test-fragment (p12* x y) 
                    #:implements p12
                    #:library (bvlib [{bvand bvnot bvule} 1]))
     
     ; Sign function. ~ 1.4 sec.
     (test-fragment (p13* x) 
                    #:implements p13
                    #:library (bvlib [{bvsz bvneg bvlshr bvashr bvor} 1])
                    #:minbv 32)
     
     ; Floor of average of two integers without over-flowing. ~ 2.5 sec.
     (test-fragment (p14* x y) 
                    #:implements p14
                    #:library (bvlib [{bv1 bvand bvlshr bvxor bvadd} 1]))
     
     ; Ceil of average of two integers without over-flowing. ~ 1 sec.
     (test-fragment (p15* x y) 
                    #:implements p15
                    #:library (bvlib [{bv1 bvor bvlshr bvxor bvsub} 1]))
     
     ; Compute max of two integers. Bug in the PLDI'11 paper: bvuge should be bvge.  
     ; ~ 1.3 sec.
     (test-fragment (p16* x y)
                    #:implements p16
                    #:library (bvlib [{bvneg bvsge bvand} 1] [{bvxor} 2]))
     
     ; Turn-off the rightmost contiguous string of 1 bits. ~ 1.8 sec.
     (test-fragment (p17* x) 
                    #:implements p17
                    #:library (bvlib [{bv1 bvand bvor bvadd bvsub} 1]))
     
     ; Test whether an unsigned integer is of the form 2^n. ~ 1.6 sec.
     (test-fragment (p18* x) 
                    #:implements p18
                    #:library (bvlib [{bvand bvredor} 2] [{bvsub bvnot bv1} 1]))
     
     ; x where m is mask which identifies field B and k 
     ; is number of bits from end of A to start of B. ~ 3.5 sec.
     (test-fragment (p19* x m k) 
                    #:implements p19
                    #:library (bvlib [{bvlshr bvshl bvand} 1] [{bvxor} 3]))
     
     )))

(time (run-tests medium-tests))
