#lang racket

(require (only-in rosette/base/core/term
                  expression expression? constant? term? get-type @app type-of)
         (only-in rosette/base/core/polymorphic
                  ite ite* ⊢ =? guarded-test guarded-value)
         (only-in rosette/base/core/real
                  @integer? @real? @= @< @<= @>= @> 
                  @+ @* @- @/ @quotient @remainder @modulo 
                  @abs @integer->real @real->integer @int?)
         (only-in rosette/base/core/bool
                  @! @&& @|| @=>)
         (only-in rosette define-symbolic* [integer? sym/integer?]
                  [* sym/*] [+ sym/+] [- sym/-]
                  [= sym/=] [< sym/<] [<= sym/<=] [> sym/>] [>= sym/>=]
                  [and sym/and])
         "common.rkt"
         )

(provide smt->mip (struct-out converter))

(struct converter (mapping-info name2sym asserts bounds objs org-objs))

; Given a list of assertions and a list of objectives,
; return MIP constraints as a converter object.
(define (smt->mip asserts objs)
  ;(pretty-display `(asserts ,asserts))
  (define t1 (current-seconds))

  ;; Storage for extra assertions.
  (define assert-bounds (list))
  (define assert-placement (list))
  (define assert-comm-at (list))
  (define assert-comm (list))

  ;; Storage for different kinds of variables which are used to check legality of the conversion.
  (define syms-need-ub (set))
  (define syms-have-ub (set))
  (define syms-have-lb (set))
  (define syms-exact (set))
  
  (define (get-sym type)
    (define-symbolic* m sym/integer?)
    (associate-type m type)
    m)
  
  (define (associate-type m type)
    (cond
      [(equal? type 'exact) (set! syms-exact (set-add syms-exact m))]
      [(equal? type 'need-ub) (set! syms-need-ub (set-add syms-need-ub m))]
      [(equal? type 'have-ub) (set! syms-have-ub (set-add syms-have-ub m))]
      [(equal? type 'have-lb) (set! syms-have-lb (set-add syms-have-lb m))]))
  
  (define old-syms (set))
  (define (hash-ref-sym h key #:lb [lb #f] #:ub [ub #f] #:type [type #f])
    (if (hash-has-key? h key)
        (hash-ref h key)
        (let ([val (get-sym type)])
          ;(pretty-display `(create ,key ,val))
          (for ([v key])
            (when (term? v) (set! old-syms (set-add old-syms v))))
          (when (or lb ub) (set! assert-bounds (cons (bound val lb ub) assert-bounds)))
          (hash-set! h key val)
          val)))

  (define upperbound (make-hash))
  (define (set-upperbound s n)
    (if (hash-has-key? upperbound s)
        (hash-set! upperbound s (min n (hash-ref upperbound s)))
        (hash-set! upperbound s n)))
  
  (define lowerbound (make-hash))
  (define (set-lowerbound s n)
    (if (hash-has-key? lowerbound s)
        (hash-set! lowerbound s (max n (hash-ref lowerbound s)))
        (hash-set! lowerbound s n)))

  ;; Collect bound
  ;; Return #f if it is a bound assertion.
  ;; Return #t otherwise.
  (define (collect-bound v)
    (match v
      [(expression (== @<=) (? integer? n) (? constant? s))
       (set-lowerbound s n) #f]
      
      [(expression (== @<) (? integer? n) (? constant? s))
       (set-lowerbound s (add1 n)) #f]
      
      [(expression (== @<=) (? constant? s) (? integer? n))
       (set-upperbound s n) #f]
      
      [(expression (== @<) (? constant? s) (? integer? n))
       (set-upperbound s (sub1 n)) #f]

      [(expression (== @&&) a b)
       (or (collect-bound a) (collect-bound b))]

      [_ #t]
      ))

  ;; Mapping of an old symbolic variable to its valid range
  (define legal-vals (make-hash))

  ;; Given an old symbolic variable, returns its vailde range (infered from the assertions).
  (define-syntax-rule (get-legal-vals s)
    (range (hash-ref lowerbound s) (hash-ref upperbound s)))

  ;; Mapping of Mv to a list of (Mvn n)
  (define mapping-sym-info (make-hash))

  ;; Given an old symbolic variable Mv that can map to a range of values,
  ;; create a set of new MIP symbolic variables Mvn such that Mvn = 0 or 1,
  ;; and Mvn = 1 iff Mv = n.
  (define (init-mapping-sym s)
    (if (hash-has-key? legal-vals s)
        (hash-ref legal-vals s)
        (let ([vals (get-legal-vals s)])
          (hash-set! legal-vals s vals)
          (let ([l (for/list ([val vals]) (get-mapping-sym-conc-no-init s val))])
            ;; sum_n(Mvn) = 1
            (set! assert-placement (cons (sym/= (apply sym/+ l) 1) assert-placement))
            ;; store mapping info to decode Mvn back to original variable Mv
            (hash-set! mapping-sym-info
                       s
                       (for/list ([new-sym l] [val vals]) (cons new-sym val)))
            )
          vals)))

  (define (has-mapping? s)
    (hash-has-key? legal-vals s))

  ;; Given Mv (s) and n, return Mvn
  (define (get-mapping-sym-conc-no-init s n)
    (hash-ref-sym mapping-matrix-hash (set s n) #:lb 0 #:ub 1 #:type 'exact)
    )

  ;; Given Mv (s) and n, return Mvn and create all Mvn's
  (define (get-mapping-sym-conc s n)
    (init-mapping-sym s)
    (hash-ref-sym mapping-matrix-hash (set s n) #:lb 0 #:ub 1 #:type 'exact)
    )

  ;; Return a communication indicator Cuvn
  ;; Cuvn = 1 if Mv (s1) != Mu (s2) and (Mv == n) or (Mu == n)
  (define (get-comm-at-conc s1 s2 n)
    (define mapping-s1 (get-mapping-sym-conc s1 n))
    (define mapping-s2 (get-mapping-sym-conc s2 n))
    ;; 0 <= Cuvn <= 1
    (define comm (hash-ref-sym mapping-matrix-hash (set s1 s2 n) #:lb 0 #:ub 1
                               #:type 'need-ub)) ;; tag with 'need-ub to check legality later
    ;; Cuvn >= Mv - Mu
    (set! assert-comm-at
          (cons (sym/<= (sym/- mapping-s1 mapping-s2) comm) assert-comm-at))
    ;; Cuvn >= Mu - Mv
    (set! assert-comm-at
          (cons (sym/<= (sym/- mapping-s2 mapping-s1) comm) assert-comm-at))
    comm
    )

  ;; Return a communication indicator Cuv
  ;; Cuv = 1 if Mv (s1) != Mu (s2)
  (define (get-comm s1 s2)
    (define range1 (init-mapping-sym s1))
    (define range2 (init-mapping-sym s2))
    (unless (equal? range1 range2)
      (raise "Not supporting for creating comm var when u and v can map to different sets"))

    ;; 0 <= Cuv <= 1
    (define comm (hash-ref-sym mapping-matrix-hash (set s1 s2) #:lb 0 #:ub 1
                               #:type 'need-ub)) ;; tag with 'need-ub to check legality later

    ;; for all n, Cuv >= Cuvn
    (for ([n range1])
      (let ([comm-at-n (get-comm-at-conc s1 s2 n)])
        (set! assert-comm (cons (sym/<= comm-at-n comm) assert-comm))))
    comm
    )

  (define used-old-syms (set))
  (define (add-old-sym v)
    (unless (set-member? used-old-syms v)
      (set! used-old-syms (set-add used-old-syms v))
      (define lb
        (if (hash-has-key? lowerbound v)
            (hash-ref lowerbound v)
            (begin
              (fprintf (current-error-port)
                       (format "Warning: no lowerbound for ~a is specified. MIP implicitly assumes that ~a >= 0.\n"
                               v v))
              #f)))
      (define ub (and (hash-has-key? upperbound v) (hash-ref upperbound v)))
      (when (or lb ub) (set! assert-bounds (cons (bound v lb ub) assert-bounds)))))
  
  ;; Given constraint s1 != s2
  (define (not-eq-constraint s1 s2)
    (cond
      [(and (has-mapping? s1) (has-mapping? s2))
       ;; if s1 (Mv) and s2 (Mu) have new corresponding MIP variable Mvn's
       ;; for all n, assert Mvn + Mun <= 1
       (raise "MIP converter: unimplemented")]
      
      [(and (or (has-mapping? s1) (set-member? used-old-syms s1))
            (or (has-mapping? s2) (set-member? used-old-syms s2)))
       (raise (format "MIP converter: cannot convert (! (= ~a ~a))" s1 s2))]

      [else #f]))

  (define mapping-matrix-hash (make-hash))
  (define (mapping-matrix v)
    ;(pretty-display `(mapping ,v))
    (match v
      ;; if Mv != Mu and (Mv == n) or (Mu == n), + x
      [(expression
        (== @+)
        (expression (== ite)
                    (expression (== @&&)
                                (expression (== @!)
                                            (expression (== @=)
                                                        (? constant? mu)
                                                        (? constant? mv)))
                                     (expression (== @=) (? constant? m1) (? integer? n)))
                    (? number? x1) (? number? x2))
        (expression (== ite)
                    (expression (== @&&)
                                (expression (== @!)
                                            (expression (== @=) mu vm))
                                (expression (== @=) (? constant? m2) n))
                    x1 x2))
       ;(pretty-display `(MATCH-1 ,x1 ,x2 n))
       (if (or (and (equal? m1 mu) (equal? m2 mv))
               (and (equal? m2 mu) (equal? m1 mv)))
           (let ([muvn (get-comm-at-conc mu mv n)])
             (sym/+ (sym/* x1 muvn) (sym/* x2 (sym/- 1 muvn)))
             )
           v)
       ]

      ;; if Mv == n, + x
      [(expression (== ite)
                   (expression (== @=) (? integer? n) (? constant? mv))
                   (? number? n1) (? number? n2))
       ;(pretty-display `(MATCH-2))
       (define mvn (get-mapping-sym-conc mv n))
       (sym/+ (sym/* n1 mvn) (sym/* n2 (sym/- 1 mvn)))
       ;; This transformation doesn't require upperbound.
       ]
      
      ;; if Mv == Mu, + x
      [(expression (== ite)
                   (expression (== @=) (? constant? mu) (? constant? mv))
                   (? number? n1) (? number? n2))
       ;(pretty-display `(MATCH-3))
       (define comm (get-comm mu mv))
       (sym/+ (sym/* n1 (sym/- 1 comm)) (sym/* n2 comm))
       ]

      ;; Mv != Mu
      [(expression (== @!) (expression (== @=) (? constant? v1) (? constant? v2)))
       (not-eq-constraint v1 v2)]

      ;; simple bounds
      ;[(expression (or (== @<) (== @<=)) (? constant?) (? number?)) v]
      ;[(expression (or (== @<) (== @<=)) (? number?) (? constant?)) v]
      
      ;; others
      [(expression op es ...)
       (apply expression op (for/list ([e es]) (mapping-matrix e)))]

      [(? constant?)
       (when (set-member? old-syms v) (raise (format "A removed old variable ~a is used." v)))
       (add-old-sym v)
       v]
      
      [_ v])
    )

  (define assert-obj (list))
  (define (convert-objective v type)
    (if (expression? v)
        (let ([obj (get-sym type)]
              [new-v (mapping-matrix v)])
          ;; If an objective v is an expression, introduce a new variable,
          ;; and assert it equal to the expression. This is requied for CPLEX.
          (set! assert-obj (cons (expression @= new-v obj) assert-obj))
          obj)
        (begin
          (associate-type v type)
          v)))

  ;; Remove assertions on bounds of old symbolic variables.
  (define (remove-old-syms v)
    (match v
      [(expression op (? constant? a) (? number? b))
       (not (and (member op (list @< @<=)) (set-member? old-syms a)))]
      
      [(expression op (? number? b) (? constant? a))
       (not (and (member op (list @< @<=)) (set-member? old-syms a)))]

      [_ #t]))

  ;; Test if v is of form (= term1 term2)
  (define (not-eq? v)
    (match v
      [(expression (== @!) (expression (== @=) (? constant? v1) (? constant? v2))) #t]
      [_ #f]))

  (define (bound-as-bound b)
    (let ([lb (bound-lb b)])
      (and lb (< lb 0))))

  (define (bound-as-assert b) (not (bound-as-bound b)))

  ;; Collect bounds and remove those assertions
  (set! asserts (filter collect-bound asserts))

  ;; Separate (= term1 term2) constraints to the second group
  ;; because we need information from other assertions (used and removed variables)
  ;; before converting them.
  (define first-group (filter (lambda (x) (not (not-eq? x))) asserts))
  (define second-group (filter (lambda (x) (not-eq? x)) asserts))
  
  (define new-asserts (filter identity (map mapping-matrix (append first-group second-group))))

  (define new-objs
    (for/list ([o objs])
      (let* ([type (objective-type o)]
             [tag (if (equal? 'min type) 'have-ub 'have-lb)])
        (objective type (convert-objective (objective-expr o) tag)))))
  
  ;(set! new-asserts (filter remove-old-syms new-asserts))

  #|
  (pretty-display `(bounds ,assert-bounds))
  (pretty-display `(placement ,assert-placement))
  (pretty-display `(comm-at ,assert-comm-at))
  (pretty-display `(comm ,assert-comm))
  (pretty-display `(asserts ,new-asserts))
  
  (pretty-display `(objective ,assert-obj))
  (pretty-display `(minimize ,minimize))
  (pretty-display `(maximize ,maximize))
|#


  ;; Check legality
  (define name2sym
    (legal-conversion (append new-asserts assert-obj)
                      syms-need-ub syms-have-ub syms-have-lb syms-exact))

  (define all-asserts
    (append (filter bound-as-assert assert-bounds)
            assert-placement assert-comm-at assert-comm
            new-asserts assert-obj
            ))
  
  (define t2 (current-seconds))
  (fprintf (current-error-port) (format "Converting time: ~a\n" (- t2 t1)))
  (converter mapping-sym-info name2sym all-asserts (filter bound-as-bound assert-bounds) new-objs objs)
  )

(define (legal-conversion all-asserts syms-need-ub syms-have-ub syms-have-lb syms-exact)
  ;;(fprintf (current-error-port) (format "New vars: ~a ~a ~a ~a\n" syms-need-ub syms-have-ub syms-have-lb syms-exact))

  (define name2sym (make-hash))
  
  (define-syntax-rule (set-add! x a)
    (set! x (set-add x a)))

  (define (collect-syms v)
    (define need-ub-pos (set))
    (define need-ub-neg (set))
    (define have-ub-pos (set))
    (define have-ub-neg (set))
    (define (f v pos)
      (match v
        [(expression (or (== @+) (== @*)) es ...)
         (for ([e es]) (f e pos))]
        [(expression (== @-) es ...)
         (f (car es) pos)
         (for ([e (cdr es)]) (f e (not pos)))]
        [(? constant?)
         (hash-set! name2sym (get-name v) v)
         (cond
           [(set-member? syms-need-ub v)
            (if pos (set-add! need-ub-pos v) (set-add! need-ub-neg v))]
           
           [(set-member? syms-have-ub v)
            (if pos (set-add! have-ub-pos v) (set-add! have-ub-neg v))]
           
           [(set-member? syms-have-lb v)
            (if pos (set-add! have-ub-neg v) (set-add! have-ub-pos v))]

           [(set-member? syms-exact v) (void)]
           ;[else (raise (format "Illegal old variable ~a in new assertion." v))]

           )]
        [_ (void)]))
    (f v #t)
    (values need-ub-pos need-ub-neg have-ub-pos have-ub-neg))

  (define (check-top v)
    ;(pretty-display `(check-top ,v))
    (match v
      [(expression (== @&&) es ...)
       (for ([e es]) (check-top e))]
      
      [(expression op lhs rhs)
       (cond
         [(member op (list @<= @< @=))
          (define-values (lhs-need-ub-pos
                          lhs-need-ub-neg
                          lhs-have-ub-pos
                          lhs-have-ub-neg)
            (collect-syms lhs))
          
          (define-values (rhs-need-ub-pos
                          rhs-need-ub-neg
                          rhs-have-ub-pos
                          rhs-have-ub-neg)
            (collect-syms rhs))

          (define lhs-need-ub (set-union lhs-need-ub-pos rhs-need-ub-neg))
          (define rhs-need-ub (set-union rhs-need-ub-pos lhs-need-ub-neg))
          (define lhs-have-ub (set-union lhs-have-ub-pos rhs-have-ub-neg))
          (define rhs-have-ub (set-union rhs-have-ub-pos lhs-have-ub-neg))

          ;; LHS: Need UB & Have UB
          (when (and (not (set-empty? lhs-need-ub)) (not (set-empty? lhs-have-ub)))
            (raise (format "Illegal conversion (1) ~a in ~a" lhs-need-ub v)))
          ;; RHS: Need UB & Have UB
          (when (and (not (set-empty? rhs-need-ub)) (not (set-empty? rhs-have-ub)))
            (raise (format "Illegal conversion (2) ~a in ~a" rhs-need-ub v)))
                
          (if (member op (list @<= @<))
              (begin
                ;; RHS: Need UB
                (when (not (set-empty? rhs-need-ub))
                  (pretty-display `(v ,v))
                  (pretty-display `(set ,lhs-need-ub ,rhs-need-ub ,lhs-have-ub ,rhs-have-ub))
                  (raise (format "Illegal conversion (3) ~a in ~a" rhs-need-ub v))))

              (begin
                ;; LHS & RHS: Need UB
                (when (and (not (set-empty? lhs-need-ub)) (not (set-empty? rhs-need-ub)))
                  (raise (format "Illegal conversion (4) ~a in ~a" lhs-need-ub v)))
                ;; LHS: Need UB, RHS: don't have UB
                (when (and (not (set-empty? lhs-need-ub)) (set-empty? rhs-have-ub))
                  (raise (format "Illegal conversion (5) ~a in ~a" lhs-need-ub v)))
                ;; RHS: Need UB, LHS: don't have UB
                (when (and (not (set-empty? rhs-need-ub)) (set-empty? lhs-have-ub))
                  (raise (format "Illegal conversion (6) ~a in ~a" rhs-need-ub v)))))
          ]
         [else (raise (format "Illegal assertion (1) ~a" v))])
       ]

      [_ (raise (format "Illegal assertion (2) ~a" v))]))

  (map check-top all-asserts)
  name2sym
  )