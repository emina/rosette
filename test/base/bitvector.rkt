#lang racket

(require rackunit rackunit/text-ui rosette/lib/util/roseunit
         racket/fixnum 
         rosette/base/core/term
         rosette/base/core/bool
         rosette/base/core/bitvector
         rosette/base/core/merge
         (only-in rosette/base/form/define define-symbolic)
         "common.rkt")

(define BV (bitvector 4))
(define-symbolic x y z BV)

(define (naive op)
  (case (procedure-arity op)
    [(1) (lambda (x) (if (bv? x) (op x) (expression op x)))]
    [(2) (lambda (x y)
           (cond [(and (bv? x) (bv? y)) (op x y)]
                 [else (expression op x y)]))]
    [else(lambda xs
           (if (andmap bv? xs) (apply op xs) (apply expression op xs)))]))
  
(define (naive* op)
  (case (procedure-arity op)
    [(2) (lambda (x y)
           (cond [(and (bv? x) (bv? y)) (op x y)]
                 [(bv? x) (expression op x y)]
                 [(bv? y) (expression op y x)]
                 [(term<? x y) (expression op x y)]
                 [else (expression op y x)]))]
    [else (lambda xs
            (if (andmap bv? xs) 
                (apply op xs)
                (let-values ([(lits terms) (partition bv? xs)])
                  (if (null? lits)
                      (apply expression op (sort terms term<?))
                      (apply expression op (apply op lits) (sort terms term<?))))))]))

(define (key e [cache (make-hash)])
  (match e
    [(expression o t ...)
     (list* o (map (curryr key cache) t))]
    [(? constant?)
     (hash-ref! cache e (hash-count cache))]
    [_ e]))

(define (break-symmetries es)
  (define keys (make-hash))
  (define (seen? e)
    (let ([k (key e)])
      (or (hash-has-key? keys k)
          (hash-ref! keys k #f))))
  (for/list ([e es] #:unless (seen? e)) e)) 

(define (exprs ops subexprs)
  (apply
   append
   (for/list ([op ops])
     (case (procedure-arity op)
       [(1) (remove-duplicates (map op subexprs))]
       [(2) (remove-duplicates
             (for*/list ([x subexprs][y subexprs])
               (op x y)))]
       [else (remove-duplicates
              (append 
               (for*/list ([x subexprs][y subexprs])
                 (op x y))
               (for*/list ([x subexprs][y subexprs][z subexprs])
                 (op x y z))))]))))


;(define e0 (list x y z))
;(define e1 (exprs (list (naive* @bvand) (naive* @bvor) (naive @bvnot)) e0))
;(define e2 (exprs (list (naive* @bvand) (naive* @bvor) (naive @bvnot)) (append e0 e1)))