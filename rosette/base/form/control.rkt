#lang racket

(require "../core/eval.rkt" "../core/store.rkt" "../core/result.rkt"
         "../core/term.rkt" "../core/equality.rkt" 
         "../core/merge.rkt" "../core/bool.rkt")

(provide @if @and @or @not @nand @nor @xor @implies
         @unless @when @cond @case else)
  
(define-syntax (@if stx)
  (syntax-case stx ()
    [(_ test-expr then-expr else-expr)
     (quasisyntax/loc stx
       (branch-and-merge test-expr
                         (thunk then-expr) 
                         (thunk else-expr)))]))

(define (branch-and-merge test-expr then-branch else-branch)
  (define test (@true? test-expr))
  (cond [(eq? test #t) (then-branch)]
        [(eq? test #f) (else-branch)]
        [else (eval-guarded! (list test (! test)) (list then-branch else-branch))]))

(define-syntax (@and stx)
  (syntax-case stx ()
    [(_) (syntax/loc stx #t)]
    [(_ arg) (syntax/loc stx arg)]
    [(_ arg0 arg ...) (syntax/loc stx (@if arg0 (@and arg ...) #f))]))

(define-syntax (@or stx)
  (syntax-case stx ()
    [(_) (syntax/loc stx #f)]
    [(_ arg) (syntax/loc stx arg)]
    [(_ arg0 arg ...) 
     (quasisyntax/loc stx (let ([val arg0]) (@if val val (@or arg ...))))]))

(define @not @false?)

(define-syntax (@implies stx)
  (syntax-case stx ()
    [(_ x y) (syntax/loc stx (@if x y #t))]))

(define-syntax (@nor stx)
  (syntax-case stx ()
    [(_ expr ...) (syntax/loc stx (@not (@or expr ...)))]))

(define-syntax (@nand stx)
  (syntax-case stx ()
    [(_ expr ...) (syntax/loc stx (@not (@and expr ...)))]))

(define (@xor a b)
  (@if a (@if b #f a) b))

(define-syntax (@unless stx)
  (syntax-case stx ()
    [(_ test body ...) (syntax/loc stx (@if test (void) (let () body ...)))]))

(define-syntax (@when stx)
  (syntax-case stx ()
    [(_ test body ...) (syntax/loc stx (@if test (let () body ...) (void)))]))

(define-syntax (@cond stx)
  (syntax-case stx (else)
    [(_) (syntax/loc stx (void))]
    [(_ [else else-val ...]) (syntax/loc stx (let () else-val ...))]
    [(_ [then0 then0-val ...] [then then-val ...] ...) 
     (syntax/loc stx (@if then0 
                              (let () then0-val ...) 
                              (@cond [then then-val ...] ...)))]))

(define-syntax (@case stx)
  (syntax-case stx (else)
    [(_ expr 
        [(then-val ...) then-expr ...] ... 
        [else else-expr ...]) 
     (syntax/loc stx 
       (let ([tmp expr])
         (@cond [(@or (@equal? tmp (quote then-val)) ...) then-expr ...] ...
                [else else-expr ...])))]
    [(_ expr
        [(then-val ...) then-expr ...] ...)
     (syntax/loc stx 
       (@case expr 
              [(then-val ...) then-expr ...] ...
              [else (void)]))]))
