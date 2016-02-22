#lang racket

(require syntax/id-table 
         (for-syntax "syntax-properties.rkt")  
         "syntax-properties.rkt" 
         (only-in rosette constant))

(provide (all-defined-out))

(define context (make-parameter 
                 '()
                 (lambda (v) (cons v (context)))))

(define (in-context anchor ctx closure)
  (define loc (format "~a:~a:~a:~a:~a" 
                      (syntax-source ctx)
                      (syntax-line ctx)
                      (syntax-column ctx)
                      (syntax-position ctx) 
                      (syntax-span ctx)))
  (parameterize ([context (cons anchor loc)]) (closure)))

; Creates a constant of the given type that is 
; identified by the current context.  Repeated 
; call to this procedure with the same context and type 
; return equal? results.
(define (context->constant type)
  (constant (caar (context)) (map cdr (context)) type))

; Creates a list of n constants of the given type, 
; which are identified by the current context.  Repeated 
; call to this procedure with the same context, type, and 
; n return equal? results.
(define (context->constants type n)
  (define id (caar (context)))
  (define path (map cdr (context)))
  (for/list ([i n]) (constant id (cons i path) type)))

(define codegen (make-free-id-table null #:phase 0))

; Defines a macro that introduces a new kind of hole in Rosette.
; Recursive holes are defined with the #:depth keyword, specifying
; the base and recursive case.  Plain holes can specify any number of 
; patterns, the same way as would be done for syntax-rules.  The 
; optional id-gen procedure takes as input an invocation of the id 
; macro and a solution, and produces the resulting syntax, based on 
; the current (context).
(define-syntax (define-synthax stx)
  (syntax-case stx ()
    [(_ id ([(_ p0 ... #:depth 0) e0] 
            [(_ pk ... #:depth k) ek])) 
     (syntax/loc stx 
       (define-synthax id ([(_ p0 ... #:depth 0) e0] 
                           [(_ pk ... #:depth k) ek])
         (lambda (expr sol)
           (syntax-case expr ()
             [(_ p0 ... #:depth 0) (syntax/source e0)]
             [(_ pk ... #:depth k) (syntax/source ek)]))))]
    [(_ id ([(_ p0 ... #:depth 0) e0] 
            [(_ pk ... #:depth k) ek]) 
        id-gen)
     (syntax/loc stx
       (begin
         (define-syntax (id stx)
           (syntax-case stx ()
             [(call pk ... #:depth k)
              (quasisyntax/loc stx 
                (in-context #'call (syntax/source call) 
                            (thunk #,(if (<= (eval #'k) 0) 
                                         (syntax/source e0) 
                                         (syntax/source ek)))))]))                    
         (free-id-table-set! codegen #'id id-gen)))]
    [(_ id ([(_ pat ...) expr] ...) id-gen)
     (syntax/loc stx
       (begin
         (define-syntax (id stx)
           (syntax-case stx ()
             [(call pat ...) 
              (quasisyntax/loc stx 
                (in-context #'call (syntax/source call) 
                            (thunk #,(syntax/source expr))))] ...))                  
         (free-id-table-set! codegen #'id id-gen)))]))



;(define-synthax ??
;  ([(_) (?? (context))]
;   [(_ t) t])
;  (lambda (expr sol) #t))
;
;(define-synthax choose
;  ([(_ x) (begin0 x (printf "~a\n" (context)))]
;   [(_ x y ...) (list x y ...)])
;  (lambda (expr sol) 
;    (syntax-case expr ()
;      [(_ x) #'x]
;      [_ #f])))
;
;(define-synthax rc 
;  ([(_ #:depth 0) (printf "~a\n" (context))]
;   [(_ #:depth k) (rc #:depth (sub1 k))]))
;
;(define (ch3) (choose (choose (choose 3))))

;(define-synthax choose
;  ([(_ x) 
;    (begin0 x (printf "~a: ~a\n" x (context)))]
;   [(_ x y ...) 
;    (begin0 (list (choose x) y ...) (printf "~a: ~a\n" (list x y ...) (context)))]))
;(choose 1)
;(choose 1 2 3 4)
;(choose (choose 2))


