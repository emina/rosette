#lang racket

(require rackunit rackunit/text-ui rosette/lib/roseunit
         racket/fixnum
         rosette/base/core/term
         rosette/base/adt/list
         rosette/base/core/bool
         rosette/base/core/real
         rosette/base/core/merge
         rosette/base/core/union
         (only-in rosette/base/form/define define-symbolic)
         "common.rkt")

(define-symbolic x y z @integer?)

(define-symbolic a b c @boolean?)


(define l0 (merge a (list 1 2 3) (list 4 5 6)))
(define l1 (merge b (list 1) (list 2 3 4 5)))
(define l2 (merge c (list 1 2 3) x))

(define (check-null?)
  (check-equal? (@null? '()) #t)
  (check-equal? (@null? (cons 1 2)) #f)
  (check-equal? (@null? l0) #f)
  (check-equal? (@null? l2) #f)
  (check-equal? (@null? (merge a '() y)) a))

(define (check-cons)
  (check-match? (@cons l1 l2) 
                (union : 
                     [(== c) (list (union : [(== b) (list 1)] [(== (! b)) (list 2 3 4 5)]) 1 2 3)] 
                     [(== (! c)) (cons (union : [(== b) (list 1)] [(== (! b)) (list 2 3 4 5)]) x)]))
  (check-union? (@cons y l2) {  [c `(,y 1 2 3)] [(! c) (cons y x)]})
  (check-match? (@cons l2 y) (cons l2 y)) 
  (check-equal? (@cons x y) (cons x y)))

(define (check-car) 
  (check-equal? (@car (cons x y)) x)
  (check-equal? (@car (list x y)) x)
  (check-equal? (@car (@cons l0 l2)) l0))

(define (check-cdr) 
  (check-equal? (@cdr (cons x y)) y)
  (check-equal? (@cdr (list x y)) (list y))
  (check-equal? (@cdr (@cons l0 l2)) l2))

(define (check-length)
  (check-equal? (@length (list 1 2 3)) 3)
  (check-equal? (@length l0) 3)
  (check-equal? (@length l1) (merge b 1 4))
  (check-equal? (@length (union-filter l2 @list?)) (merge* (cons c 3))))

(define (check-list-ref) 
  (check-equal? (@list-ref (list 1 2 3) 1) 2)
  (check-equal? (@list-ref l1 0) (merge* (cons b 1) (cons (! b) 2)))
  (check-equal? (@list-ref l1 1) (merge* (cons (! b) 3)))
  
  (check-equal? (@list-ref (list 1 2 3) x) (merge* (cons (@= x 0) 1) (cons (@= x 1) 2) (cons (@= x 2) 3)))
  (check-equal? (@list-ref (list a b) x) (merge*  (cons (@= x 0) a) (cons (@= x 1) b)))
  
  (check-equal? (@list-ref l1 x) (merge* (cons  b (@list-ref (list 1) x))
                                          (cons (! b) (@list-ref (list 2 3 4 5) x)))))
  
(define (check-append) 
  (set! l2 (merge c (list 1 2 3) (list a b)))
  (check-equal? (@append l2) l2)
  (check-equal? (@append l2 (list)) l2)
  (check-equal? (@append (list) l2) l2)
  (check-equal? (@append (list) l2 (list)) l2)
  (check-equal? (@append (list 1 2) (list 3 4)) (list 1 2 3 4))
  (check-equal? (@append (list 1 2 3) l0) `(1 2 3 ,(merge a 1 4) ,(merge a 2 5) ,(merge a 3 6)))  
  (check-equal? (@append l0 (list 1 2 3)) `(,(merge a 1 4) ,(merge a 2 5) ,(merge a 3 6) 1 2 3))
  (check-union? (@append l1 l2) {[(&& b c) `(1 1 2 3)] 
                                  [(&& b (! c)) `(1 ,a ,b)] 
                                  [(&& (! b) c) `(2 3 4 5 1 2 3)] 
                                  [(&& (! b) (! c)) `(2 3 4 5 ,a ,b)]} )
  (define l3 (merge a (list 1) (list 2)))
  (define l4 (merge b (list 1) (list a)))
  (define l5 (merge c (list a) (list b)))
  (check-equal? (@append l3 l4 l5) (@append l3 (@append l4 l5)))
  (check-equal? (@append l3 l4 l5) (@append (@append l3 l4) l5))
  )

(define (check-reverse)
  (check-equal? (@reverse (list 1 2 3)) (list 3 2 1))
  (check-equal? (@reverse l0) (merge a (list 3 2 1) (list 6 5 4)))) 

(define (check-flatten)
  (check-equal? (@flatten (list 1 (list 2) 3)) (flatten '(1 (2) 3)))
  (check-equal? (@flatten 'a) (flatten 'a))
  (check-equal? (@flatten l0) l0)
  (check-equal? (@flatten (merge a '(1) '((((#f)))))) `(,(merge a 1 #f)))
  (check-equal? (@flatten (merge a '(1 2 (3 4)) '(1 2 (#f 4)))) `(1 2 ,(merge a 3 #f) 4))
  (check-equal? (@flatten (merge a '(1 2 (3 4)) '(1 2 ((#f) 4)))) `(1 2 ,(merge a 3 #f) 4))
  (check-union? (@flatten (merge a '(1 2 (3 4)) '(1 2 ((3 3) 4)))) {[a '(1 2 3 4)] [(! a) '(1 2 3 3 4)]})
  (check-equal? (@flatten (merge a '((1 . 2) (((3) 4))) '(((1) 2 #f) . 4))) `(1 2 ,(merge a 3 #f) 4))
  (check-union? (@flatten (merge a '(1 (2) 3) '(4 . 5))) {[a '(1 2 3)] [(! a) '(4 5)]}))
 

(define list-tests
  (test-suite+ 
   "Tests for rosette/base/list.rkt"
   (check-null?)
   (check-cons)
   (check-car)
   (check-cdr)
   (check-length)
   (check-list-ref)
   (check-append)
   (check-reverse)
   (check-flatten)
   ))

(module+ test
  (time (run-tests list-tests)))
