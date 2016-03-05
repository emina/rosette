#lang rosette/safe




(define-values (prop:foo foo? foo-value) (make-struct-type-property	'foo))

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
   (define (hash2-proc self rec) 2)])

(define-enum suit '(club diamond heart spade))
(suit 'club)
(define-symbolic s suit?)
(label s)
(ordinal s)
(label (if b (suit 'club) 3))

(define env (solve (assert (suit<? s (suit 'diamond)))))
(evaluate s env)
