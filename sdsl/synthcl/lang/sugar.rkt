#lang rosette

(require racket/syntax syntax/id-table "operators.rkt" "forms.rkt"
         (for-template (only-in rosette #%app) "operators.rkt" "forms.rkt"))

(provide desugar += -= *= /= %= &= $= ^= <<= >>=)

(define procs (make-free-id-table #:phase 10))

(define-syntax-rule (define-compound-assignment [id op] ...)
  (begin
    (define (id stx)
      (syntax-case stx ()
        [(_ lval expr) 
         (with-syntax* ([lval* (desugar #'lval)]
                        [expr* (desugar #'expr)]
                        [rval  (syntax/loc stx (op lval* expr*))])
           (quasisyntax/loc stx (= lval* rval)))])) ...
    (dict-set! procs #'id id) ...))
                                                                        
(define-compound-assignment 
  [+= +] [-= -] [*= *] [/= /] [%= %] 
  [&= &] [$= $] [^= ^] [<<= <<] [>>= >>])

(define (desugar stx)
  (syntax-case stx ()
    [(tag expr ...)
     (and (identifier? #'tag) (dict-has-key? procs #'tag))
     ((dict-ref procs #'tag) stx)]
    [(expr ...)
     (with-syntax ([(expr* ...) (map desugar (syntax->list stx))])
       (quasisyntax/loc stx (expr* ...)))]
    [_ stx]))
