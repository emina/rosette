#lang rosette

(require "core.rkt")

(provide ?? choose)

(define-synthax ??
  ([(_)   (hole integer?)]
   [(_ t) (hole t)])
  (lambda (expr sol)
    (define val (completion sol))
    (if (term? val) expr val)))

(define-synthax choose
  ([(_ x) x]
   [(_ x y ...) (choose* (thunk x) (thunk y) ...)])
  (lambda (expr sol) 
    (syntax-case expr ()
      [(_ x) #'x]
      [(_ x ...) 
       (let* ([xs (syntax->list #'(x ...))]
              [vs (completion sol (sub1 (length xs)))])
         (if (andmap term? vs)
             expr
             (let loop ([xs xs][vs vs])
               (match* (xs vs)
                 [((list y) (list)) y]
                 [((list y _ ...) (list #t _ ...)) y]
                 [(_ _) (loop (cdr xs) (cdr vs))]))))])))
         
(define (choose* . xs)
  (let loop ([xs xs][hs (hole boolean? (sub1 (length xs)))])
    (match xs
      [(list y) (y)]
      [(list y ys ...)
       (if (car hs) 
           (y)
           (loop ys (cdr hs)))])))
