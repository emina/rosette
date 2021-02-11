#lang rosette

(require 
 rosette/lib/lift 
 (prefix-in racket/ (only-in racket string-append symbol->string regexp-match?)))

(provide (all-defined-out))

(define (symbol->string s)
  (for/all ([s s])
    (racket/symbol->string s)))

(define (regexp-match? px str)
  (for*/all ([px px][str str])
     (racket/regexp-match? px str)))

(define string-append
  (case-lambda
    [()
     (racket/string-append)]
    [(str)
     (for/all ([str str])
       (racket/string-append str))]
    [(str1 str2)
     (for*/all ([str1 str1][str2 str2])
       (racket/string-append str1 str2))]
    [strs
     (string-append (car strs) (apply string-append (cdr strs)))]))

