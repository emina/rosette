#lang racket

(require syntax/id-table 
         (for-syntax "syntax-properties.rkt")  
         "syntax-properties.rkt" 
         (only-in rosette/lib/util/syntax read-module)
         (only-in rosette constant model))

(provide (all-defined-out))

(define context (make-parameter '()
                 (lambda (v) (cons v (context)))))

(define (in-context ctx closure)
  (parameterize ([context (identifier->tag ctx)]) (closure)))

; Creates a constant of the given type that is 
; identified by the current context.  Repeated 
; call to this procedure with the same context and type 
; return equal? results.
(define (context->constant type)
  (constant (context) type))

; Creates a list of n constants of the given type, 
; which are identified by the current context.  Repeated 
; call to this procedure with the same context, type, and 
; n return equal? results.
(define (context->constants type n)
  (define path (context))
  (for/list ([i n]) (constant (cons i path) type)))

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
                (in-context (syntax/source call) 
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
                (in-context (syntax/source call) 
                            (thunk #,(syntax/source expr))))] ...))                  
         (free-id-table-set! codegen #'id id-gen)))]))

(define (identifier->tag id)
  (tag (syntax->datum id) 
       (syntax-source id)
       (syntax-line id)
       (syntax-column id)
       (syntax-position id) 
       (syntax-span id)))
       
(struct tag (name source line column position span) 
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match self
       [(tag n (? path? src) ln col _ _)
        (fprintf port "~a:~a:~a:~a" n 
                 (string-replace (path->string (file-name-from-path src)) ".rkt" "") ln col)]
       [(tag n src ln col _ _)
        (fprintf port "~a:~a:~a:~a" n src ln col)]))])

(define (tag-contains? outer inner)
  (match* (outer inner)
    [((tag _ src0 _ _ pos0 span0) (tag _ src1 _ _ pos1 span1))
     (and (equal? src0 src1)
          (<= pos0 pos1)
          (<= (+ pos1 span1) (pos0 span0)))]))

; Returns the context (if any) that make up the constant's identifier.
(define (constant->context c)
  (match c
    [(constant (and ts (list (? tag?) ...)) _) ts]
    [(constant (and ts (list _ (? tag?) ...)) _) (cdr ts)]
    [_ #f]))

; Returns the set of contexts that make up the constants in the given solution.
(define (solution->contexts sol)
  (for*/set ([k (in-dict-keys (model sol))]
             [ts (in-value (constant->context k))] #:when ts)
    ts))


(define (generate-forms sol)
  (let* ([ctxs (solution->contexts sol)]
         [roots (for/set ([ctx ctxs]) (last ctx))]
         [sources (for/set ([r roots]) (tag-source r))]
         [sources (set-map sources read-module)])
    sources))

    



