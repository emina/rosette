#lang racket

(require racket/splicing (for-syntax racket/syntax)
         syntax/parse/define
         (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
         (only-in "merge.rkt" merge merge* merge-same)
         (only-in "bool.rkt" ! || &&)
         (only-in "union.rkt" union union?)
         (only-in "term.rkt" expression)
         (only-in "polymorphic.rkt" guarded guarded-test guarded-value ite ite*)
         (only-in "equality.rkt" @equal?)
         "safe.rkt"  "../core/eval.rkt" "../core/store.rkt" "../core/result.rkt")

(provide for/all for*/all guard-apply)

; This macro is equivalent to a nested use of 
; for/all.  For example, 
; (for*/all ([v0 val0] [v1 val1]) expr)
; is equivalent to 
; (for/all ([v0 val0])
;  (for/all ([v1 val1])
;    expr))
(define-syntax-parser for*/all
  #:disable-colon-notation
  [(_ () e ...+) (syntax/loc this-syntax (begin e ...))]
  [(_ (v0:gv0 v:gv ...) e ...+)
   (syntax/loc this-syntax
     (for/all (v0:gv0)
       (for*/all (v:gv ...) e ...)))])

; This macro takes the following form:
; (for/all ([v val]) expr)
; where v is an identifier that can be used in expr,
; and val is a Rosette value.  If the provided value 
; is a symbolic reference, the macro evaluates the 
; expression for all possible v's to which that 
; symbolic reference could point.  If the provided 
; value is not a symbolic reference, then the expression 
; is simply evaluated with v bound to the value itself.
(define-syntax-parser for/all
  [(_ ([v:id val]) e ...+)
   (syntax/loc this-syntax
     (let ([proc (lambda (v) e ...)])
       (match val
         [(union gvs) (guard-apply proc gvs)]
         [other       (proc other)])))]
  [(_ ([v:id val #:exhaustive]) e ...+)
   #:with ooo (quote-syntax ...)
   (syntax/loc this-syntax
     (let ([proc (lambda (v) e ...)])
       (match val
         [(or (? union? sym) (and (expression (or (== ite) (== ite*)) _ ooo) sym))
          (guard-apply proc (flatten-guarded sym))]
         [other (proc other)])))]
  [(_ ([v:id val concrete]) e ...+)
   (syntax/loc this-syntax (for/all ([v val concrete @equal?]) e ...))]
  [(_ ([v:id val concrete ==]) e ...+)
   (syntax/loc this-syntax
     (let ([sym val] [=== ==])
       (guard-apply
        (lambda (v) e ...)
        (for/list ([c concrete]) (cons (=== sym c) c)))))])

(define (flatten-guarded v)
  (merge-same 
   (let loop ([guards '()][val v])
     (match val
       [(expression (== ite) c t e)
        (append (loop (cons c guards) t)
                (loop (cons (! c) guards) e))]
       [(expression (== ite*) gvs ...)
        (apply append
               (for/list ([gv gvs])
                 (loop (cons (guarded-test gv) guards)
                       (guarded-value gv))))]
       [(union gvs)
        (apply append
               (for/list ([gv gvs])
                 (loop (cons (car gv) guards)
                       (cdr gv))))]
       [_ (list (cons (apply && guards) val))]))))

; Applies the given procedure to each of the guarded values,
; given as guard/value structures.  The application of the procedure 
; to each value is done under the value's guard, and so are all 
; the state updates performed during the evaluation.  The result 
; of this procedure is the result of this evaluation process.  
; The guard-apply procedure also merges any state updates resulting 
; from successful guarded evaluations of proc on the given values.  
;
; At most one of the given guards may be true under any model.
(define (guard-apply proc guarded-values [guard-of car] [value-of cdr])
  ; If any of the guarded-values has #t as its guard, it's executed
  ; directly, since all the guards must be #f under all models.
  (define gv (findf (lambda (gv) (eq? (guard-of gv) #t)) guarded-values))
  (cond
    [gv   (proc (value-of gv))]
    [else (eval-guarded! (map guard-of guarded-values)
                         (map (lambda (gv) (thunk (proc (value-of gv)))) guarded-values))]))
  

  

           
