#lang racket

(require "term.rkt" "union.rkt" "bool.rkt")

(provide 
 ite ite* ⊢ guarded guarded-test guarded-value =?    
 generic-merge generic-merge*
 T*->T T*->boolean?
 sort/expression
 simplify*)


; A generic typing procedure for a lifted operator that takes N > 0 arguments of type T
; and returns a value of type T. Specifically, it assumes that at least one value passed 
; to it is typed, and it returns the type T of the first given typed value. See term.rkt.
(define T*->T 
  (case-lambda 
    [(x) (get-type x)]
    [(x y) (or (and (typed? x) (get-type x)) (get-type y))]
    [xs (for/first ([x xs] #:when (typed? x)) (get-type x))]))


; Polymorphic operators and procedures that are shared by 
; multiple primitive types.
(define-operator =? 
  #:identifier '=? 
  #:range T*->boolean?
  #:unsafe (lambda (x y)
             (match* (x y)
               [((not (? term?)) (not (? term?))) (eq? x y)] 
               [((not (? term?)) (? term?)) (expression =? x y)]
               [((? term?) (not (? term?))) (expression =? y x)]
               [((? term?) (? term?)) (or (equal? x y)
                                          (if (term<? x y)
                                              (expression =? x y)
                                              (expression =? y x)))])))

; A generic ite operator that takes a boolean condition and 
; two values v1 and v2. The values v1 and vn must be of the same 
; primitive type T.  That is, (type-of v1 v2) = T for some pritimive 
; type T. This operator is intended only for internal use and should not 
; be called by client code. 
(define-operator ite 
  #:identifier   'ite  
  #:range   (lambda (b t f) (type-of t f))
  #:unsafe (lambda (b t f)
             (match* (b t f)
               [((? boolean?) _ _) (if b t f)]
               [(_ _ (== t)) t]
               [(_ (expression (== ite) (== b) x _) _) (ite b x f)]
               [(_ (expression (== ite) (== (! b)) _ y) _) (ite b y f)]
               [(_ _ (expression (== ite) (== b) _ y)) (ite b t y)]
               [(_ _ (expression (== ite) (== (! b)) x _)) (ite b t x)]
               [(_ _ _) (expression ite b t f)])))


; A generic operator that takes a boolean condition and a value, and it evaluates 
; to that value if the condition is true.  Otherwise, its output is undefined.
(define (make-guarded g v) (expression ⊢ g v))

(define-operator ⊢
  #:identifier '⊢
  #:range (lambda (g v) (type-of v))
  #:unsafe make-guarded)

(define-match-expander guarded
  (lambda (stx)
    (syntax-case stx ()
      [(_ g-pat v-pat) #'(expression (== ⊢) g-pat v-pat)]))
  (syntax-id-rules ()
    [(_ g v) (make-guarded g v)]
    [_ make-guarded]))

(define (guarded-test gv) 
  (match gv [(expression (== ⊢) g _) g]))

(define (guarded-value gv) 
  (match gv [(expression (== ⊢) _ v) v]))

; A generic ite* operator that takes one or more guard-value pairs, 
; (g1 . v1) ... (gn . vn), and merges them into a single value 
; of the form (ite* (guarded g1 v1) ...(guarded g1 v1)). All guards must be 
; symbolic @boolean? terms.  All values v1 ... vn must be of the same 
; primitive type T.  That is, (type-of v1 ... vn) = T for some pritimive 
; type T. This operator is intended only for internal use and should not 
; be called by client code. The operator simply sorts its arguments by 
; guard and wraps the resulting list into an expression with ite* as the 
; operator.
(define-operator ite*
  #:identifier   'ite*
  #:range   (lambda gvs (apply type-of gvs))
  #:unsafe (lambda gvs 
             (match gvs
               [(list (cons _ a)) a]
               [(list (cons a b) (cons (expression (== @!) a) c)) (ite a b c)]
               [(list (cons (expression (== @!) a) c) (cons a b)) (ite a b c)]
               [(list (app simplify-ite (cons a b)) (app simplify-ite (cons c d)))
                (cond [(equal? b d) b]
                      [(term<? a c) (expression ite* (guarded a b) (guarded c d))]
                      [else (expression ite* (guarded c d) (guarded a b) )])]
               [(list (app simplify-ite (cons a b)) (app simplify-ite cs) ...)
                (cond [(for/and ([c cs]) (equal? b (cdr c))) b]
                      [else (apply 
                             expression 
                             ite* 
                             (sort (cons (guarded a b) (for/list ([c cs]) (guarded (car c) (cdr c))))
                                   term<? 
                                   #:key guarded-test))])])))
    

; A generic eager merging procedure that takes a list of guard-value pairs, 
; ps = '((g1 . v1) ... (gn . vn)), and merges them into a single value 
; of the form (ite* (guarded g1 v1) ... (guarded g1 v1)). All guards must be 
; symbolic @boolean? terms.  All values v1 ... vn must be of the same 
; type T.  That is, (type-of v1 ... vn ∅) = T for some primitive type T.
(define (generic-merge* ps)
  (match ps
    [(list _) ps]
    [(list (cons a _) (cons b _)) (list (cons (|| a b) (apply ite* ps)))]
    [(list (cons a _) ...) (list (cons (apply || a) (apply ite* ps)))]))


; A generic eager merging procedure that takes a list of guard-value pairs, 
; ps = '((g1 . v1) ... (gn . vn)), and merges them into a single value 
; of the form (⊕ (ite g1 v1 ∅) ... (ite gn vn ∅)). All guards must be 
; symbolic @boolean? terms.  All values v1 ... vn must be of the same 
; type T, which is also the type of the empty value ∅.  That is, 
; (type-of v1 ... vn ∅) = T.  The procedure ⊕ must be an op? with the 
; signature (op/-> (#:rest T) T). 
(define (generic-merge ⊕ ∅ ps)
  (match ps
    [(list _) ps]
    [(list (cons g a) (cons (expression (== @!) g) b)) (list (cons #t (ite g a b)))]
    [(list (cons (expression (== @!) g) b) (cons g a)) (list (cons #t (ite g a b)))]
    [(or (list (cons (expression (== @&&) g h) x) (cons (expression (== @&&) g f) y)) 
         (list (cons (expression (== @&&) g h) x) (cons (expression (== @&&) f g) y)) 
         (list (cons (expression (== @&&) h g) x) (cons (expression (== @&&) g f) y)) 
         (list (cons (expression (== @&&) h g) x) (cons (expression (== @&&) f g) y)))
     (list (cons g (match* (h f)
                     [(_ (expression (== @!) h)) (ite h x y)]
                     [((expression (== @!) f) _) (ite f y x)]
                     [(_ _) (⊕ (ite h x ∅) (ite f y ∅))])))]
    [(list (app simplify-ite (cons g x)) (app simplify-ite (cons h y))) 
     (list (cons (|| g h) (if (equal? x y) x (⊕ (ite g x ∅) (ite h y ∅)))))]
    [(list (app simplify-ite (cons a x)) (app simplify-ite (cons b y)) ...)
     (list (cons (apply || a b)
                 (if (for/and ([z y]) (equal? x z))
                     x
                     (apply ⊕ (ite a x ∅) (map (curryr ite ∅) b y)))))]))

(define (simplify-ite p)
  (match* ((car p) (cdr p))
    [(a (expression (== ite) a x _)) (cons a x)]
    [(a (expression (== ite) (expression (== @!) a) _ x)) (cons a x)]
    [((expression (== @!) a) (expression (== ite) a _ x)) (cons a x)]
    [(_ _) p]))



; Sorts the arguments to the given binary operator and returns the resulting expression.
(define (sort/expression @op x y) 
  (cond [(not (term? x)) (expression @op x y)]
        [(not (term? y)) (expression @op y x)]
        [(term<? x y) (expression @op x y)]
        [else (expression @op y x)]))

; Applies the given simplification function to the given list until 
; no more simplifications can be made.  The simplification function should 
; take as input 2 values and return either #f (if no simplification is possible)
; or the simplified result of applying f to those values.  The optional limit 
; value determines when the list is too big for simplification---in which case, 
; simplify* acts as the identity function on xs.  The limit is 100 by default.
(define (simplify* xs f [limit 100])
  (if (> (length xs) limit)
      xs
      (let ([out (let outer ([xs xs])
                   (match xs
                     [(list x rest ..1)
                      (let inner ([head rest] [tail '()])
                        (match head
                          [(list) (cons x (outer tail))]
                          [(list y ys ...)
                           (match (f x y)
                             [#f (inner ys (cons y tail))]
                             [v (outer (cons v (append ys tail)))])]))]
                     [_ xs]))])
        (if (= (length out) (length xs)) out (simplify* out f)))))            

      
