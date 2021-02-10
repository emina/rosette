#lang rosette

(require rosette/lib/angelic rosette/lib/destruct)


(define BV (bitvector 8))

(struct Add (arg) #:transparent)
(struct Mul (arg) #:transparent)
(struct Sqr ()    #:transparent)

(define (interpret prog [acc (bv 0 BV)])
  (if (null? prog)
      acc
      (interpret
       (cdr prog)
       (destruct (car prog)
         [(Add v) (bvadd acc v)]
         [(Mul v) (bvmul acc v)]
         [(Sqr)   (bvmul acc acc)]
         [_       acc]))))

(define (inst*)
  (define-symbolic* arg BV)
  (choose* (Add arg) (Mul arg) (Sqr)))

(define (prog* n)
  (if (<= n 0)
      (list)
      (cons (inst*) (prog* (- n 1)))))

(define-symbolic acc BV)

(define prog (prog* 3))

(define sol
  (synthesize
   #:forall (list acc)
   #:guarantee
   (assert
    (equal? (interpret prog acc)
            (bvsub (bvmul (bv 3 BV) acc acc) (bv 1 BV))))))

(evaluate prog sol)
  
   
 
      
