#lang rosette

(require rosette/lib/synthax rosette/guide/scribble/util/demo)

(provide bvmul2_?? bvmul2_choose bvmul2_bitfast
         bvsdiv2_bitcmp bvsdiv2_bvcmp bvsdiv2_bvcmp*)

(define (bvmul2_?? x) 
  (bvshl x (?? (bitvector 8))))

(define (bvmul2_choose x)
  ((choose bvshl bvashr bvlshr) x (?? (bitvector 8))))

(define-grammar (bitfast y)
  [expr
   (choose y (?? (bitvector 8))
           ((bop) (expr) (expr)))]
  [bop
   (choose bvshl bvashr bvlshr
           bvand bvor bvxor
           bvadd bvsub)])

(define (bvmul2_bitfast x)
  (bitfast x #:depth 2))

(define-grammar (bitcmp y)
  [cmp
   (choose
    ((op) (bitfast y) (bitfast y))
    (and (cmp) (cmp)))]
  [op
   (choose
    bvult bvule
    bvslt bvsle)])

(define (bvsdiv2_bitcmp x)
  (if (bitcmp x)
      (bitfast x)
      (bitfast x)))

(define (bvsdiv2 x)
  (if (bvsge x (bv 0 8))
      (bvlshr x (bv 1 8))
      (bvadd (bvand x (bv 1 8))
             (bvashr x (bv 1 8)))))

(define-grammar (bvcmp xs)
  [cmp
   (choose
    ((op) xs xs)
    (and (cmp) (cmp)))]
  [op
   (choose
    bvult bvule
    bvslt bvsle)])

(define (bvsdiv2_bvcmp x)
  (if (bvcmp (bitfast x))
      (bitfast x)
      (bitfast x)))
   

(define (bveq_sketch x y)
  (bvcmp (choose x y) #:depth 2))


(define-grammar (bvcmp* xs)
  [cmp
   (choose
    ((op) (arg) (arg))
    (and (cmp) (cmp)))]
  [op
   (choose
    bvult bvule
    bvslt bvsle)]
  [arg xs])

(define (bveq_sketch* x y)
  (bvcmp* (choose x y) #:depth 2))

(define (bvsdiv2_bvcmp* x)
  (if (bvcmp* (bitfast x))
      (bitfast x)
      (bitfast x)))

(define-symbolic x (bitvector 8))


(define-demo ??-demo
  (demo (bvmul2_?? (bv 1 8)))
  (demo (bvmul2_?? (bv 3 8)))

  (demo (bvmul2_?? x))
  (demo (equal? (bvmul2_?? x) (bvmul2_?? x)))

  (define sol
    (synthesize
     #:forall (list x)
     #:guarantee (assert (equal? (bvmul2_?? x) (bvmul x (bv 2 8))))))

  (demo sol)

  (demo (print-forms sol)))

(define-demo choose-demo
  (demo (bvmul2_choose (bv 1 8)))
  (demo (bvmul2_choose (bv 3 8)))

  (demo (bvmul2_choose x))
  (demo (equal? (bvmul2_choose x) (bvmul2_choose x)))
  (clear-vc!)

  (define sol
    (synthesize
     #:forall (list x)
     #:guarantee (assert (equal? (bvmul2_choose x) (bvmul x (bv 2 8))))))

  (demo sol)

  (demo (print-forms sol)))

(define-demo grammar-demo
  
  (demo (bvmul2_bitfast x))
  (demo (equal? (bvmul2_bitfast x) (bvmul2_bitfast x)))
  (clear-vc!)
  
  (define sol
    (synthesize
     #:forall (list x)
     #:guarantee (assert (equal? (bvmul2_bitfast x) (bvmul x (bv 2 8))))))

  (demo sol)

  (demo (print-forms sol)))

(define-demo grammar-with-grammar-hole-demo
  
  (clear-vc!)

  (current-grammar-depth 2)
  (define sol
    (synthesize
     #:forall (list x)
     #:guarantee
     (assert (equal? (bvsdiv2_bitcmp x) (bvsdiv x (bv 2 8))))))

  (demo (print-forms sol)))

(define-demo grammar-with-argument-choose-hole-demo
  
  (clear-vc!)

  (define-symbolic x y (bitvector 8))

  (current-grammar-depth 2)
  
  (demo
   (synthesize
    #:forall (list x y)
    #:guarantee
    (assert (equal? (bveq_sketch x y) (bveq x y)))))

  (demo
   (print-forms
    (synthesize
     #:forall (list x y)
     #:guarantee
     (assert (equal? (bveq_sketch* x y) (bveq x y)))))))

(define-demo grammar-with-argument-bitfast-hole-demo
  
  (clear-vc!)

  (current-grammar-depth 2)
  
  (demo
   (synthesize
    #:forall (list x)
    #:guarantee
    (assert (equal? (bvsdiv2_bvcmp x) (bvsdiv x (bv 2 8))))))

  (demo
   (print-forms
    (synthesize
     #:forall (list x)
     #:guarantee
     (assert (equal? (bvsdiv2_bvcmp* x) (bvsdiv x (bv 2 8))))))))

(module+ main
  (??-demo)
  (choose-demo)
  (grammar-demo)
  (grammar-with-grammar-hole-demo)
  (grammar-with-argument-choose-hole-demo)
  (grammar-with-argument-bitfast-hole-demo))

