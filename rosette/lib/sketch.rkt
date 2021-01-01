#lang racket

(require syntax/id-table racket/stxparam 
         (for-syntax "util/syntax-properties.rkt" racket/stxparam)  
         "util/syntax-properties.rkt"
         (only-in rosette/lib/util/syntax read-module)
         (only-in rosette constant model term-cache term?
                  [boolean? @boolean?] [integer? @integer?] [if @if]))

(provide ?? choose define-synthax generate-forms print-forms)

; A tag consisting of a canonical identifier (syntax) for
; a synthax construct, and a source location at which that construct is instantiated.
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


; Creates a srcloc value that captures the source location information for 
; the given syntax object.
(define (syntax->srcloc stx)
  (srcloc (syntax-source stx) 
          (syntax-line stx) 
          (syntax-column stx)
          (syntax-position stx) 
          (syntax-span stx)))

; Returns true iff the source locations are in the same module, 
; and the position and span of the outer one subsumes those of the inner one.
(define (srcloc-contains? outer inner)
  (match* (outer inner)
    [((srcloc src0 _ _ pos0 span0) (srcloc src1 _ _ pos1 span1))
     (and (equal? src0 src1)
          (<= pos0 pos1)
          (<= (+ pos1 span1) (+ pos0 span0)))]))

; Stores the current synthax calling context represented 
; as a list of tas, where the most recent tag
; identifies the most recently instantiated synthax macro.
(define context (make-parameter '()))

; Controls the creation of contexts and hole. If a synthax
; macro is invoked within another synthax macro, then base-context
; is set to (context). Otherwise,  base-context is '().
(define-syntax-parameter base-context 
  (syntax-id-rules () [_ '()]))

; Evaluates bodies in a context obtained by extending
; the given base context with the tag for the given id.  
(define-syntax-rule (in-context id base body ...)
  (parameterize ([context (cons (identifier->tag id) base)])
    (syntax-parameterize ([base-context (syntax-id-rules () [_ (context)])])
      ;(printf "id: ~a\n context: ~a\n" id (context))
      body ...)))

 
; Returns n constants of the given type that  
; are identified by the current context. If n is
; not provided, returns a single constants; if n 
; is provided, a list of n constants is returned.
; Repeated calls to this procedure with the same 
; context, type, and n return equal? results.  
(define hole 
  (case-lambda 
    [(type)   (constant (context) type)]
    [(type n) (define path (context))
              (for/list ([i n]) (constant (cons i path) type))]))

; Returns completions for the n holes that are 
; identified by the current context. The completions are 
; extracted from the given solution. If n is not provided, returns a 
; single value; if n is provided, a list of n values is returned.
; Repeated calls to this procedure with the same context and 
; solution return equal? results.  
(define hole-value
  (case-lambda
    [(sol) (sol (context->constant (context)))]
    [(sol n) (define path (context))
             (for/list ([i n]) (sol (context->constant (cons i path))))]))

; Returns the constant in the current term-cache with the given
; identifier, or throws an error if no such constant exists.
(define (context->constant ctx)
  (hash-ref (term-cache) ctx 
            (thunk (error 'context->constant "unknown constant identifier: ~a" ctx))))


; Maps synthax identfiers to pairs, where each pair
; consists of the identifier itself and a procedure
; for generating code for that synthax from given a solution.
(define codegen (make-free-id-table null #:phase 0))

; Adds a binding from id to (cons id gen) to the codegen table.
(define (codegen-add! id gen)
  (free-id-table-set! codegen id (cons id gen)))

; Creates a tag for the given identifier, which must appear as a 
; key in the codegen table.  The id field of the produced tag is 
; the identifier from the codgen table that is free-identifier=? 
; to the input identifier.
(define (identifier->tag stx)
  (tag (car (free-id-table-ref codegen stx)) (syntax->srcloc stx)))


(define-syntax (define-synthax stx)
  (syntax-case stx ()
    [(_ id #:syntax body #:codegen gen)
     #'(begin
         (define-syntax id body)
         (codegen-add! #'id gen))]
    [(_ (id args ...) body ...)
     (andmap identifier? (syntax->list #'(args ...)))
     #'(begin
         (define impl
           (procedure-rename
            (let ()
              (define (impl call base args ...)
                (in-context call base body ...))
              impl)
            'id))
         (define-synthax id
           #:syntax
           (lambda (stx)
             (syntax-case stx ()
               [(call . e) #'(impl (syntax/source call) base-context . e)]))
           #:codegen
           (lambda (expr sol)
             (define vars (syntax->list #'(args ...)))
             (define vals (cdr (syntax->list expr)))
             #`(let (#,@(map list vars vals)) body ...))))]))

(define-synthax ??
  #:syntax
  (lambda (stx)
    (syntax-case stx ()
      [(call)      (syntax/loc stx (call @integer?))]
      [(call type) #`(in-context (syntax/source call) base-context (hole type))]))
  #:codegen 
  (lambda (expr sol)
    (define val (hole-value sol))
    (if (term? val) expr val)))

(define-synthax choose
  #:syntax
  (lambda (stx)
    (syntax-case stx ()
      [(_ x) #'x]
      [(call x y ...) #`(in-context (syntax/source call) base-context (choose-some (thunk x) (thunk y) ...))]))
  #:codegen
   (lambda (expr sol) 
    (syntax-case expr ()
      [(_ x) #'x]
      [(_ x ...) 
       (let* ([xs (syntax->list #'(x ...))]
              [vs (hole-value sol (sub1 (length xs)))])
         (if (andmap term? vs)
             expr
             (let loop ([xs xs][vs vs])
               (match* (xs vs)
                 [((list y) (list)) y]
                 [((list y _ ...) (list #t _ ...)) y]
                 [(_ _) (loop (cdr xs) (cdr vs))]))))])))
                                   
(define (choose-some . thunks)
  ((let loop ([xs thunks][hs (hole @boolean? (sub1 (length thunks)))])
     (match xs
       [(list x) x]
       [(list x xxs ...) (@if (car hs) x  (loop xxs (cdr hs)))]))))

; Returns the context suffix, if any, that makes up the constant's identifier. 
; If the identifier contains no tag suffix, returns #f.
(define (constant->context c)
  (match c
    [(constant (and ts (list (? tag?) ...)) _) ts]
    [(constant (and ts (list _ (? tag?) ...)) _) (cdr ts)]
    [_ #f]))

; Given a satisfiable solution that represents the result of a synthesis query, 
; generates a syntactic representation of the synthesized code, given as a list 
; of syntax objects.
(define (generate-forms sol)
  
  (define ctxs      ; contexts that make up the constants in sol
    (for*/set ([k (in-dict-keys (model sol))]
               [ts (in-value (constant->context k))] #:when ts)
      ts))
  
  (define synthaxes  
    (for*/hash ([ctx ctxs][t ctx])
      (values (tag-srcloc t) (tag-id t))))
  
  (define roots    
    (for/set ([ctx ctxs]) (tag-srcloc (last ctx))))
  
  (define sources 
    (for/set ([r roots]) (srcloc-source r)))

  ;(printf "*ctxs: ~a\n*synthaxes: ~a\n*roots: ~a\n*sources: ~a\n" ctxs synthaxes roots sources)
  
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

; Pretty-prints the result of (generate-forms sol). 
(define (print-forms sol)
  (for ([f (generate-forms sol)])
    (printf "~a:~a:~a\n" (syntax-source f) (syntax-line f) (syntax-column f))
    (printf "~a\n" (pretty-format (syntax->datum f)))))  



;; Stores the current synthax calling context, represented 
;; as a list of tags, where the most recent tag identifies the 
;; most recently instantiated synthax macro.
;(define dynamic-context (make-parameter '()))
;
;; Stores the current synthax expansion context, represented 
;; as a list of tags, where the most recent tag identifies the 
;; most recently instantiated synthax macro.
;(define-syntax-parameter static-context 
;  (syntax-id-rules () [_ '()]))
;
;; Evaluates (proc args ...) in a dynamic-context obtained by
;; extending the current static-context with the tag for the given id.
;(define-syntax-rule (in-context id proc args ...)
;  (let ([ctx (cons (identifier->tag id) (static-context))])
;    (syntax-parameterize ([static-context (syntax-id-rules () [_ ctx])])
;      (parameterize ([dynamic-context ctx])
;        ;(printf "id: ~a, context: ~a\n" id (dynamic-context))
;        (proc args ...)))))
;   
;
;; A tag consisting of a cannonical identifier (syntax) for a synthax construct, and a 
;; source location at which that construct is instantiated.
;(struct tag (id srcloc) 
;  #:transparent
;  #:methods gen:custom-write
;  [(define (write-proc self port mode)
;     (match self
;       [(tag n (srcloc (? path? src) ln col _ _))
;        (fprintf port "~a:~a:~a:~a" (syntax->datum n) 
;                 (string-replace (path->string (file-name-from-path src)) ".rkt" "") ln col)]
;       [(tag n (srcloc src ln col _ _))
;        (fprintf port "~a:~a:~a:~a" (syntax->datum n) src ln col)]))])
;
;
;
;; Returns n constants of the given type that  
;; are identified by the current context. If n is
;; not provided, returns a single constants; if n 
;; is provided, a list of n constants is returned.
;; Repeated calls to this procedure with the same 
;; context, type, and n return equal? results.  
;(define hole 
;  (case-lambda 
;    [(type)   (constant (dynamic-context) type)]
;    [(type n) (define path (dynamic-context))
;              (for/list ([i n]) (constant (cons i path) type))]))
;
;; Returns completions for the n holes that are 
;; identified by the current context. The completions are 
;; extracted from the given solution. If n is not provided, returns a 
;; single value; if n is provided, a list of n values is returned.
;; Repeated calls to this procedure with the same context and 
;; solution return equal? results.  
;(define hole-value
;  (case-lambda
;    [(sol) (sol (context->constant (dynamic-context)))]
;    [(sol n) (define path (dynamic-context))
;             (for/list ([i n]) (sol (context->constant (cons i path))))]))
;
;; Returns the constant in the current term-cache with the given
;; identifier, or throws an error if no such constant exists.
;(define (context->constant ctx)
;  (hash-ref (term-cache) ctx 
;            (thunk (error 'context->constant "unknown constant identifier: ~a" ctx))))
;
;; Creates a tag for the given identifier, which must appear as a 
;; key in the codegen table.  The id field of the produced tag is 
;; the identifier from the codgen table that is free-identifier=? 
;; to the input identifier.
;(define (identifier->tag stx)
;  (tag (car (free-id-table-ref codegen stx)) (syntax->srcloc stx)))
;
;; Creates a srcloc value that captures the source location information for 
;; the given syntax object.
;(define (syntax->srcloc stx)
;  (srcloc (syntax-source stx) 
;          (syntax-line stx) 
;          (syntax-column stx)
;          (syntax-position stx) 
;          (syntax-span stx)))
;
;(define codegen (make-free-id-table null #:phase 0))
;
;(define (codegen-add! id gen)
;  (free-id-table-set! codegen id (cons id gen)))
;
;(define-syntax (define-synthax stx)
;  (syntax-case stx ()
;    [(_ id #:syntax body #:codegen gen)
;     (syntax/loc stx
;       (begin
;         (define-syntax id body)
;         (codegen-add! #'id gen)))]))
;
;; Returns true iff the source locations are in the same module, 
;; and the position and span of the outer one subsumes those of the inner one.
;(define (srcloc-contains? outer inner)
;  (match* (outer inner)
;    [((srcloc src0 _ _ pos0 span0) (srcloc src1 _ _ pos1 span1))
;     (and (equal? src0 src1)
;          (<= pos0 pos1)
;          (<= (+ pos1 span1) (+ pos0 span0)))]))
;
;; Returns the context suffix, if any, that makes up the constant's identifier. 
;; If the identifier contains no tag suffix, returns #f.
;(define (constant->context c)
;  (match c
;    [(constant (and ts (list (? tag?) ...)) _) ts]
;    [(constant (and ts (list _ (? tag?) ...)) _) (cdr ts)]
;    [_ #f]))
;
;; Returns the set of contexts that make up the constants in the given solution.
;(define (solution->contexts sol)
;  (for*/set ([k (in-dict-keys (model sol))]
;             [ts (in-value (constant->context k))] #:when ts)
;    ts))
;
;; Given a satisfiable solution that represents the result of a synthesis query, 
;; generates a syntactic representation of the synthesized code, given as a list 
;; of syntax objects.
;(define (generate-forms sol)
;  
;  (define ctxs (solution->contexts sol))
;  
;  (define synthaxes 
;    (for*/hash ([ctx ctxs][t ctx])
;      (values (tag-srcloc t) (tag-id t))))
;  
;  (define roots
;    (for/set ([ctx ctxs]) (tag-srcloc (last ctx))))
;  
;  (define sources 
;    (for/set ([r roots]) (srcloc-source r)))
;
;  ;(printf "*ctxs: ~a\n*synthaxes: ~a\n*roots: ~a\n*sources: ~a\n" ctxs synthaxes roots sources)
;  
;  (define (synth? loc)
;    (for/or ([r roots])
;      (srcloc-contains? loc r)))
;  
;  (define (generate form)
;    (syntax-case form ()
;      [(e _ ...)
;       (let* ([loc (syntax->srcloc #'e)]
;              [id  (hash-ref synthaxes loc #f)])
;         (if id
;             (parameterize ([dynamic-context (cons (tag id loc) (dynamic-context))])              
;               (let ([gf ((cdr (free-id-table-ref codegen id)) form sol)])
;                 (cond [(equal? gf form) form]
;                       [else (generate gf)])))
;             (let* ([es (syntax->list form)]
;                    [gs (map generate es)])
;               (with-syntax ([(g ...) gs])
;                 (cond [(equal? es gs) form]
;                       [else (quasisyntax/loc form (g ...))])))))]
;      [_ form]))
;  
;    (apply 
;     append
;     (for/list ([source sources])
;       (syntax-case (read-module source) ()
;         [(mod _ _ (_ forms ...))
;          (free-identifier=? #'module (datum->syntax #'module (syntax->datum #'mod)))
;          (for/list ([form (syntax->list #'(forms ...))] #:when (synth? (syntax->srcloc form)))
;            (generate form))]
;         [other (error 'generate-forms "expected a module, given ~a" #'other)]))))
;
;; Pretty-prints the result of (generate-forms sol). 
;(define (print-forms sol)
;  (for ([f (generate-forms sol)])
;    (printf "~a:~a:~a\n" (syntax-source f) (syntax-line f) (syntax-column f))
;    (printf "~a\n" (pretty-format (syntax->datum f)))))  
;
;
;
;(define-synthax ??
;  #:syntax
;  (lambda (stx)
;    (syntax-case stx ()
;      [(call)      #'(call @integer?)]
;      [(call type) #'(in-context (syntax/source call) hole type)]))
;  #:codegen 
;  (lambda (expr sol)
;    (define val (hole-value sol))
;    (if (term? val) expr val)))
;
;(define-synthax choose
;  #:syntax
;  (lambda (stx)
;    (syntax-case stx ()
;      [(_ x) #'x]
;      [(call x y ...) #'(in-context (syntax/source call) choose-some (thunk x) (thunk y) ...)]))
;  #:codegen
;   (lambda (expr sol) 
;    (syntax-case expr ()
;      [(_ x) #'x]
;      [(_ x ...) 
;       (let* ([xs (syntax->list #'(x ...))]
;              [vs (hole-value sol (sub1 (length xs)))])
;         (if (andmap term? vs)
;             expr
;             (let loop ([xs xs][vs vs])
;               (match* (xs vs)
;                 [((list y) (list)) y]
;                 [((list y _ ...) (list #t _ ...)) y]
;                 [(_ _) (loop (cdr xs) (cdr vs))]))))])))
;                                   
;(define (choose-some . thunks)
;  ((let loop ([xs thunks][hs (hole @boolean? (sub1 (length thunks)))])
;     (match xs
;       [(list x) x]
;       [(list x xxs ...) (@if (car hs) x  (loop xxs (cdr hs)))]))))

;------------------------



;; A tag consisting of a canonical identifier (syntax) for
;; a synthax construct, and a source location at which that construct is instantiated.
;(struct tag (id srcloc) 
;  #:transparent
;  #:methods gen:custom-write
;  [(define (write-proc self port mode)
;     (match self
;       [(tag n (srcloc (? path? src) ln col _ _))
;        (fprintf port "~a:~a:~a:~a" (syntax->datum n) 
;                 (string-replace (path->string (file-name-from-path src)) ".rkt" "") ln col)]
;       [(tag n (srcloc src ln col _ _))
;        (fprintf port "~a:~a:~a:~a" (syntax->datum n) src ln col)]))])
;
;
;; Creates a srcloc value that captures the source location information for 
;; the given syntax object.
;(define (syntax->srcloc stx)
;  (srcloc (syntax-source stx) 
;          (syntax-line stx) 
;          (syntax-column stx)
;          (syntax-position stx) 
;          (syntax-span stx)))
;
;; Returns true iff the source locations are in the same module, 
;; and the position and span of the outer one subsumes those of the inner one.
;(define (srcloc-contains? outer inner)
;  (match* (outer inner)
;    [((srcloc src0 _ _ pos0 span0) (srcloc src1 _ _ pos1 span1))
;     (and (equal? src0 src1)
;          (<= pos0 pos1)
;          (<= (+ pos1 span1) (+ pos0 span0)))]))
;
;; Stores the current synthax calling context represented 
;; as a list of tas, where the most recent tag
;; identifies the most recently instantiated synthax macro.
;(define context (make-parameter '()))
;
;; Stores the source location of the body of the macro
;; identified by the first context tag (if any).
;(define scope (make-parameter #f))
;
;(define-syntax-parameter in-synthax? 
;  (syntax-id-rules () [_ #f]))
;
;(define (in-context id scope proc . args)
;  (let* ([id-tag (identifier->tag id)]
;         [base  (if scope (context) '())])
;    (parameterize ([context (cons id-tag base)])
;      (printf "id: ~a\n context: ~a\n scope: ~a\n" id (context) scope)
;      (apply proc args))))
;
;#;(define (in-context id body-loc proc . args)
;  (let* ([id-tag (identifier->tag id)]
;         [base (match (scope)
;                 [#f  (context)]
;                 [loc (if (srcloc-contains? loc (tag-srcloc id-tag)) (context) '())])])
;    (parameterize ([context (cons id-tag base)]
;                   [scope body-loc])
;      ;(printf "id: ~a\n context: ~a\n scope: ~a\n" id (context) (scope))
;      (apply proc args))))
;  
;
;; Returns n constants of the given type that  
;; are identified by the current context. If n is
;; not provided, returns a single constants; if n 
;; is provided, a list of n constants is returned.
;; Repeated calls to this procedure with the same 
;; context, type, and n return equal? results.  
;(define hole 
;  (case-lambda 
;    [(type)   (constant (context) type)]
;    [(type n) (define path (context))
;              (for/list ([i n]) (constant (cons i path) type))]))
;
;; Returns completions for the n holes that are 
;; identified by the current context. The completions are 
;; extracted from the given solution. If n is not provided, returns a 
;; single value; if n is provided, a list of n values is returned.
;; Repeated calls to this procedure with the same context and 
;; solution return equal? results.  
;(define hole-value
;  (case-lambda
;    [(sol) (sol (context->constant (context)))]
;    [(sol n) (define path (context))
;             (for/list ([i n]) (sol (context->constant (cons i path))))]))
;
;; Returns the constant in the current term-cache with the given
;; identifier, or throws an error if no such constant exists.
;(define (context->constant ctx)
;  (hash-ref (term-cache) ctx 
;            (thunk (error 'context->constant "unknown constant identifier: ~a" ctx))))
;
;
;; Maps synthax identfiers to pairs, where each pair
;; consists of the identifier itself and a procedure
;; for generating code for that synthax from given a solution.
;(define codegen (make-free-id-table null #:phase 0))
;
;; Adds a binding from id to (cons id gen) to the codegen table.
;(define (codegen-add! id gen)
;  (free-id-table-set! codegen id (cons id gen)))
;
;; Creates a tag for the given identifier, which must appear as a 
;; key in the codegen table.  The id field of the produced tag is 
;; the identifier from the codgen table that is free-identifier=? 
;; to the input identifier.
;(define (identifier->tag stx)
;  (tag (car (free-id-table-ref codegen stx)) (syntax->srcloc stx)))
;
;(define-syntax (define-synthax stx)
;  (syntax-case stx ()
;    [(_ id #:syntax body #:codegen gen)
;     (syntax/loc stx
;       (begin
;         (define-syntax id body)
;         (codegen-add! #'id gen)))]))
;
;
;; Returns the context suffix, if any, that makes up the constant's identifier. 
;; If the identifier contains no tag suffix, returns #f.
;(define (constant->context c)
;  (match c
;    [(constant (and ts (list (? tag?) ...)) _) ts]
;    [(constant (and ts (list _ (? tag?) ...)) _) (cdr ts)]
;    [_ #f]))
;
;; Given a satisfiable solution that represents the result of a synthesis query, 
;; generates a syntactic representation of the synthesized code, given as a list 
;; of syntax objects.
;(define (generate-forms sol)
;  
;  (define ctxs      ; contexts that make up the constants in sol
;    (for*/set ([k (in-dict-keys (model sol))]
;               [ts (in-value (constant->context k))] #:when ts)
;      ts))
;  
;  (define synthaxes  
;    (for*/hash ([ctx ctxs][t ctx])
;      (values (tag-srcloc t) (tag-id t))))
;  
;  (define roots    
;    (for/set ([ctx ctxs]) (tag-srcloc (last ctx))))
;  
;  (define sources 
;    (for/set ([r roots]) (srcloc-source r)))
;
;  ;(printf "*ctxs: ~a\n*synthaxes: ~a\n*roots: ~a\n*sources: ~a\n" ctxs synthaxes roots sources)
;  
;  (define (synth? loc)
;    (for/or ([r roots])
;      (srcloc-contains? loc r)))
;  
;  (define (generate form)
;    (syntax-case form ()
;      [(e _ ...)
;       (let* ([loc (syntax->srcloc #'e)]
;              [id  (hash-ref synthaxes loc #f)])
;         (if id
;             (parameterize ([context (cons (tag id loc) (context))])              
;               (let ([gf ((cdr (free-id-table-ref codegen id)) form sol)])
;                 (cond [(equal? gf form) form]
;                       [else (generate gf)])))
;             (let* ([es (syntax->list form)]
;                    [gs (map generate es)])
;               (with-syntax ([(g ...) gs])
;                 (cond [(equal? es gs) form]
;                       [else (quasisyntax/loc form (g ...))])))))]
;      [_ form]))
;  
;    (apply 
;     append
;     (for/list ([source sources])
;       (syntax-case (read-module source) ()
;         [(mod _ _ (_ forms ...))
;          (free-identifier=? #'module (datum->syntax #'module (syntax->datum #'mod)))
;          (for/list ([form (syntax->list #'(forms ...))] #:when (synth? (syntax->srcloc form)))
;            (generate form))]
;         [other (error 'generate-forms "expected a module, given ~a" #'other)]))))
;
;; Pretty-prints the result of (generate-forms sol). 
;(define (print-forms sol)
;  (for ([f (generate-forms sol)])
;    (printf "~a:~a:~a\n" (syntax-source f) (syntax-line f) (syntax-column f))
;    (printf "~a\n" (pretty-format (syntax->datum f)))))  
;
;(define-for-syntax (syntax->srcloc-list stx)
;  (list (syntax-source stx) 
;        (syntax-line stx) 
;        (syntax-column stx)
;        (syntax-position stx) 
;        (syntax-span stx)))
;
;(define-synthax ??
;  #:syntax
;  (lambda (stx)
;    (syntax-case stx ()
;      [(call)      (syntax/loc stx (call @integer?))]
;      ;[(call type) #`(in-context (syntax/source call) (srcloc #,@(syntax->srcloc-list stx)) hole type)]))
;      [(call type) #`(let ([scope in-synthax?])
;                       (syntax-parameterize ([in-synthax? (syntax-id-rules () [_ #t])])
;                         (in-context (syntax/source call) scope hole type)))]))
;  #:codegen 
;  (lambda (expr sol)
;    (define val (hole-value sol))
;    (if (term? val) expr val)))
;
;(define-synthax choose
;  #:syntax
;  (lambda (stx)
;    (syntax-case stx ()
;      [(_ x) #'x]
;      ;[(call x y ...) #`(in-context (syntax/source call) (srcloc #,@(syntax->srcloc-list stx)) choose-some (thunk x) (thunk y) ...)]))
;      [(call x y ...)
;       #`(let ([scope in-synthax?])
;           (syntax-parameterize ([in-synthax? (syntax-id-rules () [_ #t])])
;             (in-context (syntax/source call) scope choose-some (thunk x) (thunk y) ...)))]))
;  #:codegen
;   (lambda (expr sol) 
;    (syntax-case expr ()
;      [(_ x) #'x]
;      [(_ x ...) 
;       (let* ([xs (syntax->list #'(x ...))]
;              [vs (hole-value sol (sub1 (length xs)))])
;         (if (andmap term? vs)
;             expr
;             (let loop ([xs xs][vs vs])
;               (match* (xs vs)
;                 [((list y) (list)) y]
;                 [((list y _ ...) (list #t _ ...)) y]
;                 [(_ _) (loop (cdr xs) (cdr vs))]))))])))
;                                   
;(define (choose-some . thunks)
;  ((let loop ([xs thunks][hs (hole @boolean? (sub1 (length thunks)))])
;     (match xs
;       [(list x) x]
;       [(list x xxs ...) (@if (car hs) x  (loop xxs (cdr hs)))]))))