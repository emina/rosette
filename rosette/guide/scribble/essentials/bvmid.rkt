#lang rosette

(require (only-in racket/sandbox with-deep-time-limit))
(require rosette/solver/smt/z3)
(require rosette/lib/synthax)


(provide (all-defined-out))
  
(define int32? (bitvector 32))

(define (int32 i)
  (bv i int32?))

(define (bvmid lo hi)
  (bvsdiv (bvadd lo hi) (int32 2)))

(define (bvmid-no-overflow lo hi)
  (bvadd lo (bvsdiv (bvsub hi lo) (int32 2))))

(define (check-mid impl lo hi)        
  (assume (bvsle (int32 0) lo))                  
  (assume (bvsle lo hi))              
  (define mi (impl lo hi))           
  (define diff                        
    (bvsub (bvsub hi mi)             
           (bvsub mi lo)))
  (assert (bvsle lo mi))
  (assert (bvsle mi hi))
  (assert (bvsle (int32 0) diff))     
  (assert (bvsle diff (int32 1))))

(define-symbolic l h int32?)

(define-grammar (fast-int32 x y)
  [expr (choose
         x y (?? int32?) 
         ((bop) (expr) (expr))
         ((uop) (expr)))]
  [bop (choose bvadd bvsub bvand bvor bvxor bvshl bvlshr bvashr)]
  [uop (choose bvneg bvnot)])

(define (bvmid-fast lo hi)
  (fast-int32 lo hi #:depth 2))

(define (bvmid-and? lo hi)
  (equal? (fast-int32 l h #:depth 1) (fast-int32 l h #:depth 1)))

(define (check-sqrt impl n)
  (assume (bvsle (int32 0) n))
  (define √n (impl l)) 
  (define √n+1 (bvadd √n (int32 1)))
  (assert (bvule (bvmul √n √n) n))
  (assert (bvult n (bvmul √n+1 √n+1))))

(define (check-mid-slow impl lo hi)        
  (assume (bvsle (int32 0) lo))                  
  (assume (bvsle lo hi))
  (assert
   (equal?
    (bitvector->integer (impl lo hi))
    (quotient (+ (bitvector->integer lo) (bitvector->integer hi)) 2))))

(define-syntax-rule (show form)
  (begin
    (printf "> ~a\n" 'form)
    (with-handlers ([exn:fail? (lambda (e)
                                 (eprintf "~a\n" (exn-message e))
                                 (vc-clear!)
                                 (solver-shutdown (current-solver)))])
      (let ([v form])
        (unless (void? v)
          (printf "~a\n" v))))))

(define-syntax-rule (define-demo id form ...)
  (define (id)
    (printf "\n--------~a--------\n" 'id)
    form ...))

(define-demo check-mid-demo
  (show (check-mid bvmid (int32 0) (int32 0)))
  (show (check-mid bvmid (int32 0) (int32 1)))
  (show (check-mid bvmid (int32 0) (int32 2)))
  (show (check-mid bvmid (int32 10) (int32 10000))))

(define-demo verify-demo  
  (define cex (time (verify (check-mid bvmid l h))))
  (show cex)
  (define cl (evaluate l cex))
  (define ch (evaluate h cex))
  (show cl)
  (show ch)
  (show (bvmid cl ch))
  (show (check-mid bvmid cl ch))
  (show (verify (check-mid bvmid-no-overflow l h))))

(define-demo synthesize-demo
  (define sol
    (time
     (synthesize
      #:forall (list l h)
      #:guarantee (check-mid bvmid-fast l h))))
  (show (dict-count (model sol)))
  (show sol)
  (show (print-forms sol)))

(define-demo solve-demo
  (define (bvmid-fast lo hi)
    (bvlshr (bvadd hi lo) (bv #x00000001 32)))
  (show
   (print-forms 
    (time
     (synthesize
      #:forall (list l h)
      #:guarantee
      (begin
        (assume (not (equal? l h)))
        (assume (bvsle (int32 0) l))
        (assume (bvsle l h))
        (assert
         (<=> (bvmid-and? l h)
              (equal? (bvand l h) (bvmid-fast l h))))))))))

(define-demo slowdown-demo
  (show (time (verify (check-mid bvmid l h))))
  (show (time (verify (check-mid-slow bvmid l h))))
  (show (time (verify (check-mid bvmid-no-overflow l h))))
  (show (with-deep-time-limit 10 (verify (check-mid-slow bvmid-no-overflow l h)))))

(define-demo current-bitwidth-64-demo
  (parameterize ([current-bitwidth 64])
    (show (time (verify (check-mid-slow bvmid l h))))
    (show (time (verify (check-mid-slow bvmid-no-overflow l h))))))

(define-demo current-bitwidth-32-demo
  (parameterize ([current-bitwidth 32])
    (show (time (verify (check-mid-slow bvmid l h))))
    (show (time (verify (check-mid-slow bvmid-no-overflow l h))))))

(define-demo solver-options-demo
  (show (current-solver (z3 #:logic 'QF_BV)))
  (show (time (verify (check-mid bvmid l h))))
  (show (time (verify (check-mid-slow bvmid l h))))
  (current-solver (z3)))

(define-demo infinite-loop-demo
  (define (bvsqrt n)
    (cond
      [(bvult n (int32 2)) n]
      [else
       (define s0 (bvshl (bvsqrt (bvlshr n (int32 2))) (int32 1)))
       (define s1 (bvadd s0 (int32 1)))
       (if (bvugt (bvmul s1 s1) n) s0 s1)]))

  (show (bvsqrt (int32 3)))
  (show (bvsqrt (int32 4)))
  (show (bvsqrt (int32 15)))
  (show (bvsqrt (int32 16)))
  (show (with-deep-time-limit 10 (bvsqrt l))))

(define-demo sound-finitization-demo
  (define fuel (make-parameter 5))
  
  (define-syntax-rule
    (define-bounded (id param ...) body ...)
    (define (id param ...)
      (assert (> (fuel) 0) "Out of fuel") ; <--- no false negatives
      (parameterize ([fuel (sub1 (fuel))])
        body ...)))

  (define-bounded (bvsqrt n)
    (cond
      [(bvult n (int32 2)) n]
      [else
       (define s0 (bvshl (bvsqrt (bvlshr n (int32 2))) (int32 1)))
       (define s1 (bvadd s0 (int32 1)))
       (if (bvugt (bvmul s1 s1) n) s0 s1)]))

  (show (time (verify (check-sqrt bvsqrt l))))
  (show (fuel 16))
  (show (time (verify (check-sqrt bvsqrt l)))))

(define-demo complete-finitization-demo
  (define fuel (make-parameter 5))
  
  (define-syntax-rule
    (define-bounded (id param ...) body ...)
    (define (id param ...)
      (assume (> (fuel) 0) "Out of fuel") ; <--- no false positives
      (parameterize ([fuel (sub1 (fuel))])
        body ...)))

  (define-bounded (bvsqrt n)
    (cond
      [(bvult n (int32 2)) n]
      [else
       (define s0 (bvshl (bvsqrt (bvlshr n (int32 2))) (int32 1)))
       (define s1 (bvadd s0 (int32 1)))
       (if (bvugt (bvmul s1 s1) n) s0 s1)]))

  (show (time (verify (check-sqrt bvsqrt l))))
  (show (fuel 16))
  (show (time (verify (check-sqrt bvsqrt l)))))

(module+ main
  (check-mid-demo)
  (verify-demo)
  (synthesize-demo)
  (solve-demo)
  (slowdown-demo)
  (current-bitwidth-64-demo)
  (current-bitwidth-32-demo)
  (solver-options-demo)
  (infinite-loop-demo)
  (sound-finitization-demo)
  (complete-finitization-demo)
)
