#lang racket

(require (only-in rosette/base/core/term
                  expression expression? constant? term? get-type @app type-of)
         (only-in rosette/base/core/polymorphic
                  ite ite* ⊢ =? guarded-test guarded-value)
         (only-in rosette/base/core/bool
                  @! @&& @|| @=>)
         (only-in rosette/base/core/real
                  @integer? @real? @= @< @<= @>= @> 
                  @+ @* @- @/ @quotient @remainder @modulo 
                  @abs @integer->real @real->integer @int?)
         )

(provide simplify simplify-expression)

(define (simplify asserts)
  ;; Remove redundant assertions,
  ;; || assertion in which one of its clauses has been asserted before.
  (define t1 (current-seconds))
  (define temp (remove-redundant-or asserts))
  ;; Remove redundant checks
  (define t2 (current-seconds))
  ;(set! temp (map remove-redundant-check temp)) ;; this step is very slow.
  ;; Convert to form of sum of ite
  (define t3 (current-seconds))
  (set! temp (map sum-of-ite temp))
  (define t4 (current-seconds))
  (fprintf (current-error-port) (format "Simplifying time (remove-or, remove-ite, flatten): ~a ~a ~a\n"
                                        (- t2 t1) (- t3 t2) (- t4 t3)))
  temp
  )

(define (simplify-expression e)
  (sum-of-ite (remove-redundant-check e)))

(define (remove-redundant-or asserts)
  (define change #f)

  ;; Return v if we should keep v as is.
  ;; Return #f if v is redundant: if v is (|| a b), and a or b is in asserts.
  ;; Return v' if we want to simplify v to v'.
  (define (keep-assert v asserts)
    (define (search e)
      (for/or ([a asserts]) (equal? e a)))

    (define (try-simplify v op t1 t2 e1 e2)
      ;; Simplify (|| (<= 0 sym-place$21) (= sym-place$21 sym-place$74))
      ;; to (<= 0 sym-place$21)
      ;; if there is (<= 0 sym-place$74)
      (define found2
        (cond
          [(equal? e1 t1) (search (expression op e2 t2))]
          [(equal? e1 t2) (search (expression op t1 e2))]
          [(equal? e2 t1) (search (expression op e1 t2))]
          [(equal? e2 t2) (search (expression op t1 e1))]
          [else #f]))
      (if found2 (begin (set! change #t) (expression op t1 t2)) v))
    
    (match v
      [(expression (== @||) es ...)
       (define found (for*/or ([e es] [a asserts]) (equal? e a)))
       (cond
         [found (set! change #t) #f] ;; Remove this assertion if e is already asserted.
         [else
          (match v
            [(expression (== @||)
                         (expression op t1 t2)
                         (expression (== @=) (? constant? e1) (? constant? e2)))
             (try-simplify v op t1 t2 e1 e2)]
            
            [(expression (== @||)
                         (expression (== @=) (? constant? e1) (? constant? e2))
                         (expression op t1 t2))
             (try-simplify v op t1 t2 e1 e2)]

            [else v])
          ])
       ]
      [_ v]))
  
  (define temp (filter identity (map (lambda (x) (keep-assert x asserts)) asserts)))
  (if change
      (remove-redundant-or temp)
      temp))

;; Remove ite* condition checking from expression v, when condition rm is true.
(define (remove-check rm v [pos #t] [depth 1])

  (define (recurse pre-v)
    ;; Limit the recursion depth, unless it will be very slow.
    (if (> depth 0)
        (match pre-v
          [(expression op es ...)
           (define l (for/list ([e es]) (remove-check rm e (sub1 depth))))
           (define all-prim (for/and ([e l]) (not (term? e))))
           (cond
             [(and all-prim (equal? op @+)) (apply + l)]
             [(and all-prim (equal? op @-)) (apply - l)]
             [(and all-prim (equal? op @*)) (apply * l)]
             [else
              (apply expression op l)])]
          [_ pre-v])
        pre-v))

  ;; Remove ite* before recusively simplify its children.
  (define ret
  (match v
    [(expression (== ite*)
                 (expression (== ⊢) cs bs) ...)
     (define true-body
       (for/or ([c cs] [b bs])
         (and (equal? rm c) b)))
     (or true-body v)]
    ;; Don't simplify ite, unless sum-of-ite pattern matching won't work.
    #;[(expression (== ite) c a b)
     (if (equal? rm c)
         (if pos a b)
         (if (equal? (expression @! rm) c)
             (if pos b a)
             v))]
    [_ v]))
  (recurse ret))


;; Remove ite* condition checking when possible.
(define (remove-redundant-check v)

  (define (recurse pre-v)
    (match pre-v
      [(expression op es ...)
       (apply expression op (for/list ([e es]) (remove-redundant-check e)))]
      [_ pre-v]))

  ;; Remove ite* corresponding to this ite/ite* before searching for more oppurtunities.
  (define ret
  (match v
    [(expression (== ite) c a b)
     (expression ite c (remove-check c a #t) b) ;; (remove-check c b #f)
     ]
    
    [(expression (== ite*)
                 (expression (== ⊢) cs bs) ...)
     (define l
       (for/list ([c cs] [b bs])
         (expression ⊢ c (remove-check c b))))
     (apply expression ite* l)
     ]
    [_ v]))
  (recurse ret))

;; Convert v into the form of sum of ite
(define (sum-of-ite v)
  ;(pretty-display `(sum-of-ite ,v))
  (define ret
  (match v

    ;; (ite c (+ a x) x) => (+ x (ite c a 0))
    [(expression (== ite) c (expression (== @+) a x) x) 
     (expression @+ (sum-of-ite x) (expression ite c a 0))]
    
    [(expression (== ite) c (expression (== @+) x a) x)
     (expression @+ (sum-of-ite x) (expression ite c a 0))]
    
    ;; (ite c x (+ a x)) => (+ x (ite c 0 a))
    [(expression (== ite) c x (expression (== @+) a x))
     (expression @+ (sum-of-ite x) (expression ite c 0 a))]
    
    [(expression (== ite) c x (expression (== @+) x a))
     (expression @+ (sum-of-ite x) (expression ite c 0 a))]

    ;; (ite c (+ a x) x2) => (+ x (ite c a 0))
    ;; when x2 is a simplified x because of c.
    [(expression (== ite) c
                 (expression (== @+) a (expression (== ite) c x1 x2)) x2)
     (define x (expression ite c x1 x2))
     (expression @+ (sum-of-ite x) (expression ite c a 0))]
    
    [(expression (== ite) c
                 (expression (== @+) (expression (== ite) c x1 x2) a) x2)
     (define x (expression ite c x1 x2))
     (expression @+ (sum-of-ite x) (expression ite c a 0))]
    
    ;; (ite c x1 (+ a x)) => (+ x (ite c 0 a))
    ;; when x1 is a simplified x because of c.
    [(expression (== ite) c x1 (expression (== @+) a (expression (== ite) c x1 x2)))
     (define x (expression ite c x1 x2))
     (expression @+ (sum-of-ite x) (expression ite c 0 a))]
    
    [(expression (== ite) c x1 (expression (== @+) (expression (== ite) c x1 x2) a))
     (define x (expression ite c x1 x2))
     (expression @+ (sum-of-ite x) (expression ite c 0 a))]
    
    ;; (ite (= x$1 x$2) (ite (= x$0 x$2) 0 1) (ite (= x$0 x$2) 1 2))
    ;; => (ite (= x$1 x$2) (ite (= x$0 x$2) 0 1) (+ 1 (ite (= x$0 x$2) 0 1))
    ;; => (+ (ite (= x$0 x$2) 0 1) (ite (= x$1 x$2) 0 1))
    [(expression (== ite) c1
                 (expression (== ite) c2 (? number? a1) (? number? a2))
                 (expression (== ite) c2 (? number? b1) (? number? b2)))
     
     (if (= (- b1 a1) (- b2 a2))
         (if (< a1 b1)
             (expression @+ (expression ite c2 a1 a2) (expression ite c1 0 (- b1 a1)))
             (expression @+ (expression ite c2 b1 b2) (expression ite c1 (- a1 b1) 0)))
         (raise (format "Cannot simplify nested ite (1) ~a" v)))
     ]

    ;; Mu != Mv, Mv == n or Mu == n (space for communication)
    [(expression (== ite)
                 (expression (== @=) (? constant? mu) (? constant? mv))
                 x
                 (expression (== ite)
                             (expression (== @=) (? integer? n) (? constant? m1))
                             (expression (== @+) (? integer? a)
                                         y)
                             y))

     (match y
       [(expression (== ite)
                    (expression (== @=) (== n) (? constant? m2))
                    (expression (== @+) (== a) (== x))
                    (== x))
;        (pretty-display `(MATCH-Y))
;        (pretty-display `(m1-m2 ,m1 ,m2
;                                ,(or (and (equal? m1 mu) (equal? m2 mv))
;                                     (and (equal? m2 mu) (equal? m1 mv)))))
;        (pretty-display `(t1 ,t1))
;        (pretty-display `(t2 ,t2))
;        (pretty-display `(check ,(equal? x t1) ,(equal? x t2) ,(equal? t1 t2)))
;        (compare t1 t2)
        (compact-comm-constraint v mu mv m1 m2 x a n)
        ]

       ;; x2 is not exactly the same as x becuase the top-level condition has been removed
       ;; from Rosette's trivial simplification.
       [(expression (== ite)
                    (expression (== @=) (== n) (? constant? m2))
                    (expression (== @+) (== a) (== x))
                    x2)
        (match x
          [(expression (== ite)
                       (expression (== @=) (== n) (== m2))
                       _ (== x2))
           (compact-comm-constraint v mu mv m1 m2 x a n)]
          [_ (raise (format "Cannot simplify nested ite (2.1) ~a" v))])
        ]

       ;; x2 is not exactly the same as x becuase the top-level condition has been removed
       ;; from Rosette's trivial simplification.
       [(expression (== ite)
                    (expression (== @=) (== n) (? constant? m2))
                    (expression (== @+) (== a) x2)
                    x2)
        (match x2
          [(expression (== ite)
                       (expression (== @=) (== mu) (== mv))
                       (== x) _)
           (compact-comm-constraint v mu mv m1 m2 x a n)]
          [(expression (== ite)
                       (expression (== @=) (== mv) (== mu))
                       (== x) _)
           (compact-comm-constraint v mu mv m1 m2 x a n)]
          [_ (raise (format "Cannot simplify nested ite (2.2) ~a" v))])
        ]
       
       [(expression (== ite)
                    (expression (== @=) (== n) (? constant? m2))
                    (expression (== @+) (== a) t1)
                    t2)
        (pretty-display `(MATCH-5 ,mu ,mv ,n))
        (pretty-display `(y ,y))
        (pretty-display `(x ,x))
        (pretty-display `(a ,a))
        (pretty-display `(t1 ,t1))
        (pretty-display `(t2 ,t2))
        (pretty-display `(check ,(equal? x t1) ,(equal? x t2) ,(equal? t1 t2)))
        ;;(compare t1 t2)
        ;;(compact-comm-constraint v mu mv m1 m2 x a n)
        (raise (format "Cannot simplify nested ite (2.3) ~a" v))
        ]
       
       [_ (pretty-display `(NO-MATCH))
          (raise (format "Cannot simplify nested ite (2.4) ~a" v))])
     ]
     
    [(expression (== ite) c1 (expression (== ite) c2 b21 b22) b12)
     (pretty-display `(c ,c1))
     (pretty-display `(b1 ,(expression ite c2 b21 b22)))
     (pretty-display `(b2 ,b12))
     (raise (format "Cannot simplify nested ite (3) ~a" v))
     ]
    [(expression op es ...)
     (apply expression op (for/list ([e es]) (sum-of-ite e)))
     ]
    [_ v]))
  ;(pretty-display `(ret ,v ,ret))
  ret
  )

(define (compact-comm-constraint v mu mv m1 m2 x a n)
  (if (or (and (equal? m1 mu) (equal? m2 mv))
          (and (equal? m2 mu) (equal? m1 mv)))
      (expression @+ (sum-of-ite x)
                  (expression
                   @+
                   (expression ite
                               (expression @&&
                                           (expression @! (expression @= mu mv))
                                           (expression @= mu n))
                               a 0)
                   (expression ite
                               (expression @&&
                                           (expression @! (expression @= mu mv))
                                           (expression @= mv n))
                               a 0)))
      (raise (format "Cannot simplify nested ite (compact) ~a" v))))

;;;;;;;;;;;;;;;;;;;;;;;;; Handy functions for debugging ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compare x y)
  (match x
    [(expression op1 es1 ...)
     (match y
       [(expression op2 es2 ...)
        (pretty-display `(compare ,(length es1) ,(length es2)))
        (unless (equal? op1 op2)
          (pretty-display `(op ,op1 ,op2)))
        (for ([e1 es1] [e2 es2] [id (in-naturals)])
          (unless (equal? e1 e2)
            (newline)
            (pretty-display `(id ,id))
            (pretty-display `(diff1 ,e1))
            (pretty-display `(diff2 ,e2))))
          
        ])
     ]
    [_ (void)]))
     
  