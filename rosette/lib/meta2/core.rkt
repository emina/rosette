#lang racket

(require syntax/id-table racket/stxparam
         (for-syntax "syntax-properties.rkt")  
         "syntax-properties.rkt" 
         (only-in rosette/lib/util/syntax read-module)
         (only-in rosette constant model))

(provide hole define-synthax solution->forms)

; Stores the current synthax-expansion context, represented 
; as a list of tags, where the most recent tag identifies the 
; most recently instantiated synthax macro.
(define-syntax-parameter static-context 
  (syntax-id-rules () [_ '()]))

; Stores the current synthax-calling context, represented 
; as a list of tags, where the most recent tag identifies the 
; most recently instantiated synthax macro.
(define context (make-parameter '()))

; Executes the given thunk in a context that has the 
; given ctx value as its most recent tag.
(define (in-context ctx closure)
  (parameterize ([context ctx]) 
    (closure)))

; Creates a list of n constants of the given type that  
; are identified by the current context.  Repeated 
; calls to this procedure with the same context, type, and 
; n return equal? results.
(define hole
  (case-lambda 
    [(type)   (constant (context) type)]
    [(type n) (define path (context))
              (for/list ([i n]) (constant (cons i path) type))]))

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
             [(_ p0 ... #:depth k)
              (if (= (eval #'k (tag->namespace (last (context)))) 0) 
                  (syntax/source e0) 
                  (syntax/source ek))]))))]
    [(_ id ([(_ p0 ... #:depth 0) e0] 
            [(_ pk ... #:depth k) ek]) 
        id-gen)
     (syntax/loc stx
       (begin
         (define-syntax (id stx)
           (syntax-case stx ()
             [(call pk ... #:depth k)
              (quasisyntax/loc stx 
                (let ([ctx (cons (identifier->tag (syntax/source call)) (static-context))])
                  (syntax-parameterize ([static-context (syntax-id-rules () [_ ctx])])
                    (in-context ctx
                                (thunk #,(if (<= (eval #'k) 0) 
                                             (syntax/source e0) 
                                             (syntax/source ek)))))))]))                    
         (free-id-table-set! codegen #'id (cons #'id id-gen))))]
    [(_ id ([(_ pat ...) expr] ...) id-gen)
     (syntax/loc stx
       (begin
         (define-syntax (id stx)
           (syntax-case stx ()
             [(call pat ...) 
              (quasisyntax/loc stx 
                (let ([ctx (cons (identifier->tag (syntax/source call)) (static-context))])
                  (syntax-parameterize ([static-context (syntax-id-rules () [_ ctx])])
                    (in-context ctx
                                (thunk #,(syntax/source expr))))))] ...))                  
         (free-id-table-set! codegen #'id (cons #'id id-gen))))]))

; Creates a tag for the given identifier, which must appear as a 
; key in the codegen table.  The id field of the produced tag is 
; the identifier from the codgen table that is free-identifier=? 
; to the input identifier.
(define (identifier->tag stx)
  (tag (car (free-id-table-ref codegen stx)) (syntax->srcloc stx)))

; Creates a srcloc value that captures the source location information for 
; the given syntax object.
(define (syntax->srcloc stx)
  (srcloc (syntax-source stx) 
          (syntax-line stx) 
          (syntax-column stx)
          (syntax-position stx) 
          (syntax-span stx)))

; A tag consiting of a cannonical identifier (syntax) for a synthax construct, and a 
; source location at which that construct is instantied.
(struct tag (id srcloc) 
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match self
       [(tag n (srcloc (? path? src) ln col _ _))
        (fprintf port "~a:~a:~a:~a" (syntax->datum n) 
                 (string-replace (path->string (file-name-from-path src)) ".rkt" "") ln col)]
       [(tag n (srcloc src ln col _ _))
        (fprintf port "~a:~a:~a:~a" (syntax->datum n) src ln col)]))])

; Returns a namespace for the source module of the given tag.
(define (tag->namespace t)
  (module->namespace
   (make-resolved-module-path 
    (srcloc-source (tag-srcloc t)))))

; Returns true iff the source locations are in the same module, 
; and the position and span out of first tag subsumes those of the second.
(define (srcloc-contains? outer inner)
  (match* (outer inner)
    [((srcloc src0 _ _ pos0 span0) (srcloc src1 _ _ pos1 span1))
     (and (equal? src0 src1)
          (<= pos0 pos1)
          (<= (+ pos1 span1) (+ pos0 span0)))]))

; Returns the context suffix, if any, that makes up the constant's identifier. 
; If the identifier contains no tag suffix, returns #f.
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

; Given a satisfiable solution that represents the result of a synthesis query, 
; generates a syntactic representation of the synthesized code, given as a list 
; of syntax objects.
(define (solution->forms sol)
  
  (define ctxs (solution->contexts sol))
 
  (define synthaxes 
    (for*/hash ([ctx ctxs][t ctx])
      (values (tag-srcloc t) (tag-id t))))
 
  (define roots
    (for/set ([ctx ctxs]) (tag-srcloc (last ctx))))
  
  (define sources 
    (for/set ([r roots]) (srcloc-source r)))
                                
  (define (synth? loc)
    (for/or ([r roots])
      (srcloc-contains? loc r)))
  
  (define (generate form)
    (syntax-case form ()
      [(e _ ...)
       (let* ([loc (syntax->srcloc #'e)]
              [id  (hash-ref synthaxes loc #f)])
         (if id
             (parameterize ([context (cons (tag id loc) (context))])              
               (let ([gf ((cdr (free-id-table-ref codegen id)) form sol)])
                 (cond [(equal? gf form) form]
                       [else (generate gf)])))
             (let* ([es (syntax->list form)]
                    [gs (map generate es)])
               (with-syntax ([(g ...) gs])
                 (cond [(equal? es gs) form]
                       [else (quasisyntax/loc form (g ...))])))))]
      [_ form]))
                         
  (apply 
   append
   (for/list ([source sources])
     (syntax-case (read-module source) ()
       [(mod _ _ (_ forms ...))
        (free-identifier=? #'module (datum->syntax #'module (syntax->datum #'mod)))
        (for/list ([form (syntax->list #'(forms ...))] #:when (synth? (syntax->srcloc form)))
          (generate form))]
       [other (error 'generate-forms "expected a module, given ~a" #'other)]))))

  
  

    



