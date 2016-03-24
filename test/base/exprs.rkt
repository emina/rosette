#lang racket

(require rosette/base/core/term)

(provide naive naive* reduce test-exprs)

(define (concrete? x) (not (term? x)))

; Naive implementation of a non-commutative primitive operator.
(define (naive op) 
  (case (procedure-arity op)
    [(1)  (lambda (x) (if (concrete? x) (op x) (expression op x)))]
    [(2)  (lambda (x y)
            (cond [(and (concrete? x) (concrete? y)) (op x y)]
                  [else (expression op x y)]))]
    [else (lambda xs
           (if (andmap concrete? xs) (apply op xs) (apply expression op xs)))]))

; Naive implementation of a commutative primitive operator.
(define (naive* op)
  (case (procedure-arity op)
    [(2) (lambda (x y)
           (cond [(and (concrete? x) (concrete? y)) (op x y)]
                 [(concrete? x) (expression op x y)]
                 [(concrete? y) (expression op y x)]
                 [(term<? x y) (expression op x y)]
                 [else (expression op y x)]))]
    [else (lambda xs
            (if (andmap concrete? xs) 
                (apply op xs)
                (let-values ([(lits terms) (partition concrete? xs)])
                  (if (null? lits)
                      (apply expression op (sort terms term<?))
                      (apply expression op (apply op lits) (sort terms term<?))))))]))

; Turns a naive expression into a partially reduced one.
(define (reduce e)
  (match e 
    [(expression op t ...)
     (apply op (map reduce t))]
    [_ e]))

; Generates all expression of up to (and including the given depth).  
; Depth of more than 2 takes a very long time to generate.
(define (test-exprs depth ops terminals)
  (remove-duplicates 
   (apply append (map cannonicalize (reverse (exprs depth ops terminals))))))

(define (key e [cache (make-hash)])
  (match e
    [(expression o t ...)
     (list* o (map (curryr key cache) t))]
    [(? constant?)
     (hash-ref! cache e (hash-count cache))]
    [_ e]))

(define (cannonicalize es)
  (define keys (make-hash))
  (define (seen? e)
    (let ([k (key e)])
      (or (hash-has-key? keys k)
          (hash-ref! keys k #f))))
  (for/list ([e es] #:unless (seen? e)) e)) ;(or (seen? e) (equal? e (reduce e)))) e)) 

(define (exprs depth ops terminals)
  (if (= depth 0)
      (list terminals)
      (let* ([subexprs (exprs (- depth 1) ops terminals)]
             [subexprs* (for/list ([es subexprs]) 
                          (remove-duplicates (map reduce es)))]
             [bot (remove-duplicates (apply append (cdr subexprs*)))]
             [top (remove* bot (car subexprs*))]) 
        (cons 
          (apply
           append
           (for/list ([op ops])
             (case (procedure-arity op)
               [(1)  (remove-duplicates (map op top))]
               [(2)  (remove-duplicates
                      (append (for*/list ([x top][y top]) (op x y))
                              (for*/list ([x top][y bot]) (op x y))
                              (for*/list ([x bot][y top]) (op x y))))]
               [else (remove-duplicates
                      (append 
                       (for*/list ([x top][y top]) (op x y))
                       (for*/list ([x top][y bot]) (op x y))
                       (for*/list ([x bot][y top]) (op x y))
                       (for*/list ([x top][y top][z top]) (op x y z))
                       (for*/list ([x top][y top][z top]) (op x y z))
                       (for*/list ([x top][y bot][z top]) (op x y z))
                       (for*/list ([x top][y bot][z bot]) (op x y z))
                       (for*/list ([x bot][y top][z top]) (op x y z))
                       (for*/list ([x bot][y bot][z top]) (op x y z))
                       (for*/list ([x bot][y top][z bot]) (op x y z))))])))
         subexprs))))


                    
