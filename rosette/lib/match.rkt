#lang rosette

(require (for-syntax (only-in racket/syntax generate-temporary)))

(provide (rename-out [match-symbolic match] 
                     [match*-symbolic match*] 
                     [match-lambda-symbolic match-lambda]))

(define-syntax-rule (match-lambda-symbolic [pat expr ...] ...)
  (lambda (v) (match-symbolic v [pat expr ...] ...)))

(define-syntax (match-symbolic stx)
  (syntax-case stx ()
    [(_ val [pat expr] ...)
     (with-syntax ([(parsed ...) (map parse-pattern (syntax->list #'(pat ...)))]
                   [var (if (identifier? #'val) #'val (generate-temporary))])
       (syntax/loc stx 
         (for/all ([var val]);(guarded-values val)])
           (match var 
             [parsed expr] ...))))]
    [(_ val [pat expr ...] ...)
     (syntax/loc stx (match-symbolic val [pat (begin expr ...)] ...))]))

(define-syntax (match*-symbolic stx)
  (syntax-case stx ()
    [(_ (val ...) [pat expr] ...)
     (with-syntax ([(parsed ...) (map parse-pattern (syntax->list #'(pat ...)))]
                   [(var ...) (for/list ([expr (syntax->list #'(val ...))]
                                         [tmp (generate-temporaries #'(val ...))])
                                (if (identifier? expr) expr tmp))])
       (syntax/loc stx 
         (for*/all ([var val] ...); (guarded-values val)] ...)
           (match* (var ...) 
             [parsed expr] ...))))]
    [(_ val [pat expr ...] ...)
     (syntax/loc stx (match*-symbolic val [pat (begin expr ...)] ...))]))
  
;(define (guarded-values v)
;  (if (union? v) (union-contents v) (list (cons #t v))))

(begin-for-syntax
  
  (define ops 
    (syntax->list #'(! && || => <=> = < <= > >= + - * / quotient remainder expt abs sgn 
                       << >> >>> bitwise-not bitwise-and bitwise-ior bitwise-xor)))
  
  (define (parse-pattern pat)
    (syntax-case pat ()
      [(id expr ...)
       (and (identifier? #'id) (for/or ([op ops]) (free-identifier=? #'id op)))
       (quasisyntax/loc pat (expression (== id) #,@(map parse-pattern (syntax->list #'(expr ...)))))]
      [_ pat])))
  
#|
(define-syntax-rule (define-match id native)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ val [pat expr] (... ...))
       (with-syntax ([(parsed (... ...)) (map parse-pattern (syntax->list #'(pat (... ...))))])
         (syntax/loc stx 
           (let matcher ([v val])
             (native v 
                     [(union vs) (for/all ([v vs]) (matcher v))]
                     [parsed expr] (... ...)))))]
      [(_ val [pat expr (... ...)] (... ...))
       (syntax/loc stx (id val [pat (begin expr (... ...))] (... ...)))])))

(define-match @match match)
(define-match @match* match*)
|#
