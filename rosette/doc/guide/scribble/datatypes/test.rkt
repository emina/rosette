#lang rosette/safe

(struct point (x y) #:transparent)
(eq? (point 1 2) (point 1 2))
(struct pt (x y))    
(eq? (pt 1 2) (pt 1 2))
(struct pnt (x y) #:mutable #:transparent) 
(eq? (pnt 1 2) (pnt 1 2))

(define-symbolic b boolean?)
(define p (if b (point 1 2) (point 3 4)))
(point-x p)
(point-y p)
(define sol (solve (assert (= (point-x p) 3))))
(evaluate p sol)

(define-generics viewable (view viewable))

(struct square (side) 
  #:methods gen:viewable
  [(define (view self) (square-side self))])

(struct circle (radius)
  #:transparent
  #:methods gen:viewable
  [(define (view self) (circle-radius self))])

(define q (if b (square 2) (circle 3)))
(view q)
(define sol2 (solve (assert (= (view q) 3))))
(evaluate q sol2)

#|(define-values (prop:foo foo? foo-value) (make-struct-type-property	'foo))

(struct point (x y) #:transparent #:property prop:foo 3)

(define-symbolic b boolean?)
(define p (if b (point 1 2) (point 3 4)))
(foo? p)
(foo-value p)

(eq? (point 1 2) (point 1 2))

(evaluate p (solve (assert (= (point-x p) 3))))

(struct pt (x y))
(eq? (pt 1 2) (pt 1 2))

(struct farm (x)
  #:methods gen:equal+hash
  [(define (equal-proc self f rec) (and (rec (farm-x self) (farm-x f))))
   (define (hash-proc self rec) 1)
   (define (hash2-proc self rec) 2)])|#

