#lang racket

(require rackunit rackunit/text-ui 
         rosette/solver/smt/z3  rosette/solver/solution 
         rosette/lib/util/roseunit 
         rosette/base/core/term rosette/base/core/bool
         rosette/base/core/real (except-in rosette/base/core/bitvector bv)
         rosette/base/core/finitize
         rosette/base/core/polymorphic rosette/base/core/merge 
         rosette/base/core/assert
         (only-in rosette/base/form/define define-symbolic define-symbolic*)
         (only-in rosette/base/core/equality @equal?)
         (only-in rosette/base/core/bitvector [bv @bv])
         (only-in rosette evaluate))

(define solver (new z3%))
(current-bitwidth #f)

(define bw 4)
(define minval (- (expt 2 (sub1 bw))))
(define maxval+1 (expt 2 (sub1 bw))) 
(define maxval (sub1 maxval+1))

(define BV (bitvector bw))
(define (bv v [t BV]) (@bv v t))

(define-symbolic a b c d e f g @boolean?)
(define-symbolic xi yi zi @integer?)
(define-symbolic xr yr zr @real?)
(define-symbolic xb yb zb BV)

(define (solve  . asserts)
  (send/apply solver assert asserts)
  (begin0
    (send solver solve)
    (send solver clear)))

(define (lift-solution finitized-solution finitization-map)
  (sat (for/hash ([(k v) finitization-map] #:when (constant? k))
                  (values k (if (equal? @integer? (get-type k))
                                (@bitvector->integer (finitized-solution v))
                                (finitized-solution v))))))

(define (terms t) ; produces a hashmap from each typed? subterm in t to itself
  (define env (make-hash))
  (define (rec v)
    (when (typed? v)
      (unless (hash-has-key? env v)
        (hash-set! env v v)
        (match v 
          [(expression _ x ...) (for-each rec x)]
          [_ (void)]))))
  (rec t)
  (for/hash ([(k v) env]) (values k v)))

(define (check-pure-bitvector-term t)
  (define expected (terms t))
  (define actual (for/hash ([(k v) (finitize (list t) bw)] #:when (typed? k))
                   (values k v)))
  ;(printf "expected: ~a\nactual: ~a\n" expected actual)
  (check-equal? actual expected))

(define (check-finitization-1 op x y)
  (for ([i (in-range minval maxval+1)] #:when (< (integer-length (op i)) bw))
    (define expected (op i))
    (define terms (with-asserts-only 
                   (begin (@assert (@= (op x) y))
                          (@assert (@= x i)))))
    (define fmap (finitize terms bw))
    (define fsol (apply solve (map (curry hash-ref fmap) terms)))
    (define sol (lift-solution fsol fmap))
    (check-equal? (sol y) expected)))

(define (check-bitvector->* op x y bw start end)
  (for ([v (in-range start (add1 end))])
    (define i (bv v))
    (define expected (op i))
    (define terms (with-asserts-only 
                   (begin (@assert (@= (op x) y))
                          (@assert (@bveq x i)))))
    (define fmap (finitize terms bw))
    (define fsol (apply solve (map (curry hash-ref fmap) terms)))
    (define sol (lift-solution fsol fmap))
    (check-equal? (sol y) expected)))

(define (check-integer->bitvector x y bw start end)
  (for ([i (in-range start (add1 end))])
    (define expected (@integer->bitvector i (get-type y)))
    (define terms (with-asserts-only 
                   (begin (@assert (@bveq (@integer->bitvector x (get-type y)) y))
                          (@assert (@= x i)))))
    (define fmap (finitize terms bw))
    (define fsol (apply solve (map (curry hash-ref fmap) terms)))
    (define sol (lift-solution fsol fmap))
    (check-equal? (sol y) expected)))
       
(define tests:pure-bitvector-terms
  (test-suite+
   "Tests for finitization of pure BV terms."
   (check-pure-bitvector-term (bv 0))
   (check-pure-bitvector-term xb)
   (check-pure-bitvector-term (@bvneg xb))
   (check-pure-bitvector-term (@bvadd xb yb (bv 3)))
   (check-pure-bitvector-term (@bvadd xb (@bvmul yb zb) (bv 3)))
   (check-pure-bitvector-term (@bvslt xb (@bvsdiv (bv 3) zb)))
   (check-pure-bitvector-term (@concat xb (@bvand yb zb) (bv 11)))
   (check-pure-bitvector-term (@extract 3 2 (@bvand yb zb)))
   (check-pure-bitvector-term (@zero-extend (@bvxor xb (@bvand zb yb)) (bitvector 8)))
   (check-pure-bitvector-term (@sign-extend (@bvxor xb (@bvand zb yb)) (bitvector 8)))
   ))

(define tests:real-unary-terms
  (test-suite+
   "Tests for finitization of pure Int/Real unary terms."
   (check-finitization-1 @abs xi yi)
   (check-finitization-1 @abs xr yr)
   (check-finitization-1 @- xi yi)
   (check-finitization-1 @- xr yr)
   (check-finitization-1 @integer->real xi yr)
   (check-finitization-1 @real->integer xr yi)
   (check-equal? (finitize (list (@int? xr))) (make-hash (list (cons (@int? xr) #t))))
   (check-equal? (finitize (list (@int? (@+ xr 3)))) (make-hash (list (cons (@int? (@+ xr 3)) #t))))))

(define tests:casts
  (test-suite+
   "Tests for finitization of bitvector->* and integer->bitvector casts."
   (check-bitvector->* @bitvector->integer xb yi bw minval maxval)
   (check-bitvector->* @bitvector->integer xb yi (+ bw 1) minval maxval)
   (check-bitvector->* @bitvector->integer xb yi (- bw 1) (- (expt 2 (- bw 2))) (sub1 (expt 2 (- bw 2))))
   (check-bitvector->* @bitvector->natural xb yi bw 0 maxval)
   (check-bitvector->* @bitvector->natural xb yi (+ bw 1) minval maxval)
   (check-bitvector->* @bitvector->natural xb yi (- bw 1) 0 (sub1 (expt 2 (- bw 2))))
   (check-integer->bitvector xi yb bw minval maxval)
   (check-integer->bitvector xi yb (+ bw 1) minval maxval)
   (check-integer->bitvector xi yb (- bw 1) (- (expt 2 (- bw 2))) (sub1 (expt 2 (- bw 2))))
   )) 

(time (run-tests tests:pure-bitvector-terms))
(time (run-tests tests:real-unary-terms))
(time (run-tests tests:casts))
(send solver shutdown)


