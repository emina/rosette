#lang racket

(require "../core/effects.rkt"  
         "../core/term.rkt" "../core/equality.rkt" 
         "../core/merge.rkt" "../core/bool.rkt")

(provide @if @and @or @not @nand @nor @xor @implies
         @unless @when @cond @case else)

; Symbolic conditions are handled by speculatively executing both branches,
; and then merging their results and updates to state (if any). When a branch is 
; executed speculatively, its state mutations are captured and then undone. 
; The result of the capture is a closure that can be used with a merging 
; procedure to selectively re-apply the updates.  If an error is thrown 
; during speculation, all updates are undone, but they are not captured 
; (since the branch is infeasible).  After both branches have been speculatively 
; executed, their results and updates to state are merged using the merge function.
;
; Speculative execution of either branch is guarded by the path condition, stored 
; in the pc parameter.  Parameterizing pc with a new value coinjoins that 
; value with the current path condition. If the result of the conjunction is false, 
; indicating that the branch is infeasible, an error is thrown, and the branch is 
; not executed.  The error is captured by the speculate form and later handled by 
; the merge function.
(define-syntax (@if stx)
  (syntax-case stx ()
    [(_ test-expr then-expr else-expr)
     (quasisyntax/loc stx
       (branch-and-merge test-expr
                         (thunk then-expr) 
                         (thunk else-expr)))]))

(define (branch-and-merge test-expr then-branch else-branch)
  (define test (! (@false? test-expr)))
  (cond [(eq? test #t) (then-branch)]
        [(eq? test #f) (else-branch)]
        [else 
         (let-values ([(then-val then-state) (speculate (parameterize ([pc test]) (then-branch)))]
                      [(else-val else-state) (speculate (parameterize ([pc (! test)]) (else-branch)))])
           (cond [(and then-state else-state) ; both branches feasible
                  (then-state (lambda (pre post-then) (merge test post-then pre)))
                  (else-state (lambda (post-then post-else) (merge test post-then post-else)))
                  (merge test then-val else-val)]
                 [then-state                  ; only then branch feasible
                  (@assert test "both branches infeasible")
                  (then-state select-post)
                  then-val]
                 [else-state                  ; only else branch feasible
                  (@assert (! test) "both branches infeasible")
                  (else-state select-post)
                  else-val]
                 [else                        ; neither branch feasible
                  (@assert #f "both branches infeasible")]))]))

(define (select-post pre post) post) 

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
  (merge a (merge b #f a) b))

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
    [(_ expr) (syntax/loc stx (@case expr [else (void)]))]
    [(_ expr [else else-expr ...]) (syntax/loc stx (begin expr else-expr ...))]
    [(_ expr 
        [(then-val0 ...) then-expr0 ...] 
        [(then-val ...) then-expr ...] ... 
        [else else-expr ...]) 
     (syntax/loc stx 
       (let ([tmp expr])
         (@cond [(@or (@equal? tmp (quote then-val0)) ...) then-expr0 ...]
                   [(@or (@equal? tmp (quote then-val)) ...) then-expr ...] ...
                   [else else-expr ...])))]
    [(_ expr 
        [(then-val0 ...) then-expr0 ...] 
        [(then-val ...) then-expr ...] ...)
     (syntax/loc stx 
       (@case expr 
                 [(then-val0 ...) then-expr0 ...] 
                 [(then-val ...) then-expr ...] ...
                 [else (void)]))]))
