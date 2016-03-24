#lang rosette

(require 
 rosette/lib/lift 
 (prefix-in racket/ (only-in racket string-append symbol->string regexp-match?)))

(provide (all-defined-out))

(define-lift symbol->string 
  [(symbol?) racket/symbol->string])

(define-lift regexp-match?  
  [(pregexp? string?) racket/regexp-match?])

(define-lift string-append  
  [string? (compose1 string->immutable-string racket/string-append)])
