#lang racket

(require racket/splicing (for-syntax racket/syntax) 
         (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
         (only-in "merge.rkt" merge merge*)
         (only-in "bool.rkt" ! || pc)
         (only-in "union.rkt" union)
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
(define-syntax for*/all
  (syntax-rules ()
    [(_ () expr) expr]
    [(_ ([v gv]) expr)
     (for/all ([v gv]) expr)]
    [(_ ([v0 gv0] [v gv] ...) expr)
     (for/all ([v0 gv0])
       (for*/all ([v gv] ...) expr))]))


; This macro takes the following form:
; (for/all ([v val]) expr)
; where v is an identifier that can be used in expr,
; and val is a Rosette value.  If the provided value 
; is a symbolic reference, the macro evaluates the 
; expression for all possible v's to which that 
; symbolic reference could point.  If the provided 
; value is not a symbolic reference, then the expression 
; is simply evaluated with v bound to the value itself.
(define-syntax (for/all stx)
  (syntax-case stx ()
    [(_ ([v val]) expr)
     (identifier? #'v)
     (syntax/loc stx (let ([proc (lambda (v) expr)])
                       (match val
                         [(union gvs) (guard-apply proc gvs)]
                         [v         (proc v)])))]))

; Applies the given procedure to each of the guarded values,
; given as guard/value pairs.  The application of the procedure 
; to each value is done under the value's guard, and so are all 
; the state updates performed during the evaluation.  The result 
; of this procedure is the result of this evaluation process.  
; The guard-apply procedure also merges any state updates resulting 
; from successful guarded evaluations of proc on the given values.  
;
; All given guards are required to be pairwise mutually exclusive, 
; and at least one of the guards must always evaluate to true.
(define (guard-apply proc guarded-values)
  (define-values (guards outputs states)
    (guard-speculate* proc guarded-values))
  (when (null? guards)
    (assert #f (thunk (error 'for/all "all paths infeasible"))))
  (when (ormap pair? states)
    (merge-states guards states))
  (apply merge* (map cons guards outputs)))
  
; Speculatively executes the given procedure on the provided 
; guarded values and returns three lists---guards, outputs, 
; and states---of equal length.  For each input pair (cons g v) 
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
(define (guard-speculate* proc guarded-values)
  (for/fold ([guards '()] [outputs '()] [states '()]) ([gv guarded-values])
    (define guard (car gv))
    (define val (cdr gv))
    (define-values (output state) 
      (speculate* 
       (parameterize ([pc guard]) 
         (proc val))))
    (cond [state (values (cons guard guards) (cons output outputs) (cons state states))]
          [else  (assert (! guard) (thunk (error 'for/all "all paths infeasible")))
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
           
