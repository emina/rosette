#lang racket

(require "term.rkt" "union.rkt" "op.rkt" "bool.rkt")

(provide 
 ite          ; (-> @boolean? any/c any/c any/c)
 =?           ; (-> anyc/ any/c @boolean?)
 generic-merge 
 make-cast do-cast)

(define-op =? 
  #:name '=? 
  #:type (op/-> (@any/c @any/c) @boolean?)
  #:pre  (lambda (x y) (equal? (type-of x) (type-of y)))
  #:op   (lambda (x y)
           (match* (x y)
             [((not (? term?)) (not (? term?))) (eq? x y)] 
             [((not (? term?)) (? term?)) (expression =? x y)]
             [((? term?) (not (? term?))) (expression =? y x)]
             [((? term?) (? term?)) (or (equal? x y)
                                      (if (term<? x y)
                                          (expression =? x y)
                                          (expression =? y x)))])))

(define-op ite 
  #:name 'ite  
  #:type (op/-> #:arg-type (lambda (i) (if (= i 0) @boolean? @any/c)) 
                #:out-type (lambda (b t f) (type-of t f)))
  #:pre  (lambda (b t f) (equal? (type-of t) (type-of f)))
  #:op   (lambda (b t f)
           (match* (b t f)
             [((? boolean?) _ _) (if b t f)]
             [(_ _ (== t)) t]
             [(_ (expression (== ite) (== b) x _) _) (ite b x f)]
             [(_ (expression (== ite) (== (! b)) _ y) _) (ite b y f)]
             [(_ _ (expression (== ite) (== b) _ y)) (ite b t y)]
             [(_ _ (expression (== ite) (== (! b)) x _)) (ite b t x)]
             [(_ _ _) (expression ite b t f)])))

(define-syntax-rule (do-cast v primitive-type? ... symbolic-type?)
  (match v
    [(? primitive-type?) (values #t v)] ...
    [(term _ (== symbolic-type?)) (values #t v)]
    [(union _ (== symbolic-type?)) (values #t v)]
    [(union _ (? (curryr subtype? symbolic-type?))) (values #t v)]
    [(union vs (? (curry subtype? symbolic-type?)))
     (match (union-filter v symbolic-type?)
       [(union (list (cons g u))) (values g u)]
       [r (values (apply || (union-guards r)) r)])]
    [_ (values #f v)]))

(define-syntax-rule (make-cast primitive-type? ... symbolic-type?)
  (lambda (v)
    (do-cast v primitive-type? ... symbolic-type?)))


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
    [(list (cons g a) (cons (expression (== !) g) b)) (list (cons #t (ite g a b)))]
    [(list (cons (expression (== !) g) b) (cons g a)) (list (cons #t (ite g a b)))]
    [(or (list (cons (expression (== &&) g h) x) (cons (expression (== &&) g f) y)) 
         (list (cons (expression (== &&) g h) x) (cons (expression (== &&) f g) y)) 
         (list (cons (expression (== &&) h g) x) (cons (expression (== &&) g f) y)) 
         (list (cons (expression (== &&) h g) x) (cons (expression (== &&) f g) y)))
     (list (cons g (match* (h f)
                     [(_ (expression (== !) h)) (ite h x y)]
                     [((expression (== !) f) _) (ite f y x)]
                     [(_ _) (⊕ (ite h x ∅) (ite f y ∅))])))]
    [(list (app simplify-ite (cons g x)) (app simplify-ite (cons h y))) 
     (list (cons (|| g h) (if (equal? x y) x (⊕ (ite g x ∅) (ite h y ∅)))))]
    [(list (app simplify-ite (cons a x)) (app simplify-ite (cons b y)) ...)
     (list (cons (apply || a b)
                 (if (for/and ([z y]) (equal? x y))
                     x
                     (apply ⊕ (ite a x ∅) (for/list ([z y]) (ite b z ∅))))))]))

(define (simplify-ite p)
  (match p
    [(or (cons a (expression (== ite) a x _)) 
         (cons a (expression (== ite) (expression (== !) a) _ x))
         (cons (expression (== !) a) (expression (== ite) a _ x))) 
     (cons a x)]
    [_ p]))
       

#|(define simplify-ite
  (case-lambda 
    [(p) (let* ([g (car p)]
                [v (cdr p)]
                [w (simplify-ite g v)])
           (if (equal? v w) p (cons g w)))]
    [(g v) (match* (g v)
             [(a (expression (== ite) a x _)) x]
             [(a (expression (== ite) (expression (== !) a) _ x)) x]
             [((expression (== !) a) (expression (== ite) a _ x)) x]
             [(_ _) v])]))|#
      
      
      
