#lang racket

(require syntax/id-table racket/stxparam racket/local syntax/parse
         (for-syntax "util/syntax-properties.rkt" racket/stxparam syntax/parse)  
         "util/syntax-properties.rkt"
         (only-in rosette/lib/util/syntax read-module)
         (only-in rosette constant model terms-ref term?
                  [boolean? @boolean?] [integer? @integer?] [if @if] [assert @assert]))

(provide ?? choose define-synthax define-grammar define-simple-grammar generate-forms print-forms
         (rename-out [depth current-grammar-depth])
         (for-syntax save-properties) restore-properties)

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

; Returns the constant in the current (terms) with the given
; identifier, or throws an error if no such constant exists.
(define (context->constant ctx)
  (terms-ref ctx 
             (thunk (error 'context->constant "unknown constant identifier: ~a" ctx))))


; Maps synthax identfiers to pairs, where each pair
; consists of the identifier itself and a procedure
; for generating code for that synthax from given a solution.
(define codegen (make-parameter (make-immutable-free-id-table null #:phase 0)))

; Adds a binding from id to (cons id gen) to the codegen table.
(define (codegen-add! id gen)
  (define kv 
    (match (free-id-table-ref (codegen) id #f)
      [(? cons? key/val)
       (if (equal? gen codegen-error)
           key/val
           (cons (car key/val) gen))]
      [_ (cons id gen)]))
  (codegen (free-id-table-set (codegen) (car kv) kv)))

; Creates a tag for the given identifier, which must appear as a 
; key in the codegen table.  The id field of the produced tag is 
; the identifier from the codgen table that is free-identifier=? 
; to the input identifier.
(define (identifier->tag stx)
  (tag (car (free-id-table-ref (codegen) stx)) (syntax->srcloc stx)))


(define-syntax (define-synthax stx)
  (syntax-parse stx  
    [(_ id:id #:syntax body #:codegen gen)
     #'(begin
         (define-syntax id body)
         (codegen-add! #'id gen))]
    [(_ id:id [(param:id ...) body ...] gen)
     #'(begin
         (define impl
           (procedure-rename
            (lambda (call base param ...)
              (in-context call base body ...))
            'id))
         (define-synthax id
           #:syntax
           (lambda (stx)
             (syntax-case stx ()
               [(call e (... ...))
                #'(impl (syntax/source call) base-context
                        (in-context (syntax/source call) base-context e) (... ...))]))
           #:codegen gen))]
    [(_ (id:id param:id ...) body ...)
     #'(define-synthax id
         [(param ...) body ...]
         (lambda (expr sol)
           (define vars (syntax->list #'(param ...)))
           (define vals (cdr (syntax->list expr)))
           #`(let (#,@(map list vars vals)) body ...)))]))

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
      [(call x y ...) #`(in-context (syntax/source call) base-context (choose-thunk (thunk x) (thunk y) ...))]))
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
                                   
(define (choose-thunk . thunks)
  ((let loop ([xs thunks][hs (hole @boolean? (sub1 (length thunks)))])
     (match xs
       [(list x) x]
       [(list x etc ...) (@if (car hs) x  (loop etc (cdr hs)))]))))

(define (codegen-error expr sol)
  (error 'generate-forms "cannot generate code for ~a" expr))

(define depth (make-parameter 0))

(define-syntax (define-grammar stx)
  (syntax-parse stx 
    [(_ (id:id param:id ...) [cid:id clause] ...+)
     (with-syntax ([c0 (car (syntax->list #'(cid ...)))])
       #'(define-synthax id
           #:syntax
           (lambda (stx)
             (syntax-parse stx 
               [(call param ...)           #'(call param ... #:depth (depth) #:start c0)]
               [(call param ... #:depth d) #'(call param ... #:depth d #:start c0)]
               [(call param ... #:start s) #'(call param ... #:depth (depth) #:start s)]
               [(call param ... #:depth d #:start (~and s:id (~or (~literal cid) ...)))
                #:with start (car (member #'s (syntax->list #'(cid ...)) free-label-identifier=?))
                #'(local ()
                      (define-synthax cid
                        [() (@assert (>= (depth) 0))
                            (parameterize ([depth (sub1 (depth))])
                              clause)]
                        codegen-error) ...        
                      (parameterize ([depth d])
                        (in-context (syntax/source call) base-context (start))))]))
           #:codegen
           (lambda (expr sol)
             (syntax-case expr ()
               [(_ param ... _ (... ...))
                (begin (codegen-add! #'cid (lambda (e s) #'clause)) ...)])
             (syntax-case expr ()
               [(_ (... ...) #:start s) #`(#,(car (member #'s (syntax->list #'(cid ...)) free-label-identifier=?)))]
               [_ #'(c0)]))))]))

(define-syntax (define-simple-grammar stx)
  (syntax-parse stx
    [(_ (id:id param:id ...) body)
     #'(define-grammar (id param ...)
         [id body])]))
                
     
                

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
             (parameterize ([context (cons (tag id loc) (context))]
                            [codegen (codegen)])
               (let ([gf ((cdr (free-id-table-ref (codegen) id)) form sol)])
                 (cond [(equal? gf form) form]
                       [else (generate gf)])))
             (with-handlers ([exn:fail? (lambda (ex) (quasisyntax/loc form (assert #f)))])
               (let* ([es (syntax->list form)]
                      [gs (map generate es)])
                 (with-syntax ([(g ...) gs])
                   (cond [(equal? es gs) form]
                         [else (quasisyntax/loc form (g ...))]))))))]
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
    (pretty-print (syntax->datum f))))
