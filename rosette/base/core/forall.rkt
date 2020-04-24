#lang racket

(require racket/splicing (for-syntax racket/syntax)
         syntax/parse/define
         (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
         (only-in "merge.rkt" merge merge* merge-same)
         (only-in "bool.rkt" ! || && pc)
         (only-in "union.rkt" union union?)
         (only-in "term.rkt" expression)
         (only-in "polymorphic.rkt" guarded guarded-test guarded-value ite ite*)
         (only-in "equality.rkt" @equal?)
         (only-in "effects.rkt" speculate* location=? location-final-value)
         "safe.rkt")

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

(define (all-paths-infeasible)
  (error 'for/all "all paths infeasible"))

; Applies the given procedure to each of the guarded values,
; given as guard/value structures.  The application of the procedure 
; to each value is done under the value's guard, and so are all 
; the state updates performed during the evaluation.  The result 
; of this procedure is the result of this evaluation process.  
; The guard-apply procedure also merges any state updates resulting 
; from successful guarded evaluations of proc on the given values.  
;
; All given guards are required to be pairwise mutually exclusive, 
; and at least one of the guards must always evaluate to true.
(define (guard-apply proc guarded-values [guard-of car] [value-of cdr])
  (cond
    [(andmap (compose1 boolean? guard-of) guarded-values)
     ;; Either (1) concrete is empty or (2) the value is also concrete.
     ;; This case doesn't require speculation which is expensive.
     ;; Simply search for a value in gv pairs whose guard is true
     ;; and apply proc to it if there's one, error otherwise.
     (define gv (findf guard-of guarded-values))
     (cond
       [gv (proc (value-of gv))]
       [else (assert #f all-paths-infeasible)])]
    [else
     (define-values (guards outputs states)
       (guard-speculate* proc guarded-values guard-of value-of))
     (when (null? guards) (assert #f all-paths-infeasible))
     (when (ormap pair? states)
       (merge-states guards states))
     (apply merge* (map cons guards outputs))]))
  
; Speculatively executes the given procedure on the provided 
; guarded values and returns three lists---guards, outputs, 
; and states---of equal length.  For each g/v input value  
; in guarded-values for which (proc v) terminates without an 
; error, there is an index i such that the ith element of the 
; guards list is g, the ith element of the outputs list is 
; (proc v), and the ith element of the states list is the list 
; of all states updates that were performed when executing (proc v).
; Note that all state update objects for the ith execution are  
; are unique according to location=?, but two state updates in 
; different executions may be location=?.  (That is, proc would 
; update the same location if it were called with two different 
; values.)
(define (guard-speculate* proc guarded-values [guard-of car] [value-of cdr])
  (for/fold ([guards '()] [outputs '()] [states '()]) ([gv guarded-values])
    (define guard (guard-of gv))
    (define val (value-of gv))
    (define-values (output state) (speculate* guard (proc val)))
    (cond [state (values (cons guard guards) (cons output outputs) (cons state states))]
          [else  (assert (! guard) all-paths-infeasible)
                 (values guards outputs states)])))


; Given a list of n guards and their corresponding lists of 
; state-update objects, performs an n-way merge of all updates 
; to memory locations that are encapsulated in those states.
(define (merge-states guards states)
  (define locations (remove-duplicates (apply append states) location=?))
  (define guarded-states (append-map (lambda (g sts) (map (curry cons g) sts)) guards states))
  (define max-guards-per-location (length guards))
  (define (merge-procedure gss)
    (if (= (length gss) max-guards-per-location)
        (lambda (pre post) (apply merge* gss))
        (lambda (pre post) (apply merge* (cons (! (apply || (map car gss))) pre) gss))))
  (for ([loc locations]) 
    (loc (merge-procedure 
          (for/list ([gs guarded-states] 
                     #:when (location=? loc (cdr gs))) 
            (cons (car gs) (location-final-value (cdr gs))))))))
           
