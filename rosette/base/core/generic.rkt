#lang racket

(require "term.rkt" "union.rkt" "op.rkt" "bool.rkt")

(provide 
 ite          ; (-> @boolean? any/c any/c any/c)
 =?           ; (-> anyc/ any/c @boolean?)
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
  #:type (op/->  [b t f] (@boolean? @any/c @any/c) (type-of t f))
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


      
      
      
      
