#lang s-exp rosette

(require "core.rkt")

(provide ?? choose ch3)

(define-synthax ??
  ([(_)  (context->constant integer?)]
   [(_ t) (context->constant t)])
  (lambda (expr sol) #t))

(define-synthax choose
  ([(_ x) (begin0 x (printf "~a\n" (context)))]
   [(_ x y ...) (list x y ...)])
  (lambda (expr sol) 
    (syntax-case expr ()
      [(_ x) #'x]
      [_ #f])))

(define (ch3) (choose (choose (choose 3))))