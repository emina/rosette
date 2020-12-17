#lang racket

(require (for-syntax racket/dict syntax/parse syntax/parse/define syntax/id-table (only-in racket pretty-print)
                     (only-in "../core/lift.rkt" drop@))
         racket/require racket/undefined
         (filtered-in drop@ "../adt/box.rkt")
         (for-syntax (only-in "../struct/struct.rkt" [struct @struct]) (only-in "../struct/generics.rkt" @define-generics))
         (only-in "../struct/struct.rkt" [struct @struct]) (only-in "../struct/generics.rkt" @define-generics)
         (only-in racket/splicing splicing-let splicing-let-values))

(provide @#%module-begin @#%top-interaction
         (rename-out [module @module] [module* @module*] [module+ @module+]))

(define-for-syntax orig-insp (variable-reference->module-declaration-inspector (#%variable-reference)))

(define-syntax (@#%module-begin stx)
  (syntax-case stx ()
    [(_ forms ...)
     (let* ([core (local-expand #'(#%plain-module-begin forms ...) 'module-begin (list #'module*))]
            [vars (find-mutated-vars core)]      
            [transformed (box-mutated-vars core vars)])
       ;(printf "vars:~a\n" (dict->list vars))
       ;(printf "core:\n") (pretty-print (syntax->datum core))
       ;(call-with-output-file "bad.rkt" 
       ;  (lambda (out) (parameterize ([current-output-port out])
                         ;(printf "transformed:\n") 
       ;                  (pretty-print (syntax->datum transformed))))
       ;  #:mode 'text
       ;  #:exists 'replace)
       transformed)]))

(define-syntax (@#%top-interaction stx)
  (syntax-case stx ()
    [(_ . (id rest ...))
     (and (identifier? #'id) (free-identifier=? #'id #'@define-generics))
     (syntax/loc stx (id rest ...))]
    [(_ . (id e ...))
     (and (identifier? #'id) (free-identifier=? #'id #'begin))
     (syntax/loc stx (begin (@#%top-interaction . e) ...))]
    [(_ . form)
     (let* ([core (local-expand #'form 'top-level (list))] 
            [vars (find-mutated-vars core #f)]
            [top-vars (for/list ([(var mutated?) (in-dict vars)]
                                 #:unless (or (not mutated?)
                                              (equal? 'lexical (identifier-binding var))))
                        var)]
            [transformed
             (begin (unless (null? top-vars)
                      (raise-syntax-error
                       'set!
                       "assignment disallowed;\n cannot set top-level variables" #'form #f top-vars))
                    (box-mutated-vars core vars))])
       ;(printf "core:\n~a\n" core)
       ;(printf "mutated vars\n~a\n" (dict->list vars))
       ;(printf "transformed: ~a\n" transformed)
       transformed)]))
     

(define-for-syntax (find-mutated-vars form [define=>mutable? #f] [tbl (make-free-id-table)])
  (define (fmv/list lstx) (for ([stx (syntax->list lstx)]) (fmv stx)))
  (define (fmv stx)
    (syntax-parse 
     stx
     #:literal-sets (kernel-literals)
     [(set! v e)
      (fmv #'e)
      (dict-set! tbl #'v #t)]
     ;; forms with expression subforms
     [(define-values vars expr) 
      (fmv #'expr)
      (when define=>mutable? 
        (for ([v (syntax->list #'vars)])
          (dict-set! tbl v #t)))]
     [(#%expression e) (fmv #'e)]
     [(#%plain-app . rest) (fmv/list #'rest)]
     [(begin . rest) (fmv/list #'rest)]
     [(begin0 . rest) (fmv/list #'rest)]
     [(#%plain-lambda _ . rest) (fmv/list #'rest)]
     [(case-lambda (_  rest ...) ...) (fmv/list #'(rest ... ...))]
     [(if . es) (fmv/list #'es)]
     [(with-continuation-mark . es) (fmv/list #'es)]
     [(let-values ([_ e] ...) b ...) (fmv/list #'(b ... e ...))]
     [(letrec-values ([_ e] ...) b ...) (fmv/list #'(b ... e ...))]
     [(letrec-syntaxes+values _ ([_ e] ...) b ...) (fmv/list #'(b ... e ...))]
     [(#%plain-module-begin . forms) (fmv/list #'forms)]
     ;; all the other forms don't have any expression subforms (like #%top)
     [_ (void)]))
  (fmv form)
  tbl)



(define-for-syntax (lexical? id) (eq? (identifier-binding id) 'lexical))

(define-for-syntax (formals->identifiers stx)
  (syntax-parse 
   stx
   [var:id (list stx)]
   [(var:id ...) (syntax->list stx)]
   [(var:id ... . rest:id) (syntax->list #'(var ... rest))]))

(begin-for-syntax
  (define-simple-macro (quasisyntax* orig-stx new-stx)
    (let ([orig-stx* orig-stx]
          [new-stx* (quasisyntax new-stx)])
      (datum->syntax new-stx* (syntax-e new-stx*) orig-stx* orig-stx*))))

(define-for-syntax (box-mutated-vars form tbl)
  (define varref-tbl (make-free-id-table))
  (define (mutated? id) (free-id-table-ref tbl id #f))
  (define (any-mutated? ids) (for/or ([id ids]) (mutated? id)))
  (define (bmv/list lstx)
    (let* ([stxs (syntax->list lstx)]
           [fs (map bmv stxs)])
      (values (equal? fs stxs) fs)))
  
  (define (bmv/rest stx lit lstx)
    (let-values ([(pure? forms) (bmv/list lstx)])
      (if pure? stx (quasisyntax* stx (#,lit #,@forms)))))
  
  (define (bmv/proc-body formals rest)
    (let-values ([(pure? fs) (bmv/list rest)]
                 [(vs) (formals->identifiers formals)])
      (cond [(any-mutated? vs)
             #`(#,@(for/list ([v vs] #:when (mutated? v))
                     #`(set! #,v (box #,v)))
                #,@fs)]
            [pure? rest]
            [else fs])))
  
  (define (bmv stx) 
    (syntax-parse 
     (syntax-disarm stx orig-insp) 
     #:literal-sets (kernel-literals)
     [var:id
      (cond [(and (mutated? #'var) (lexical? #'var)) (quasisyntax* stx (unbox var))]
            [else #'var])]
     [(set! var expr) 
      (let ([e (bmv #'expr)])
        (cond [(lexical? #'var) (quasisyntax* stx (set-box! var #,e))]
              [(eq? e #'expr) stx]
              [else (quasisyntax* stx (set! var #,e))]))]
     [(define-values (var) expr)
      (let ([e (bmv #'expr)])
        (cond [(mutated? #'var) 
               (with-syntax ([(loc) (generate-temporaries #'(var))])
                 (dict-set! varref-tbl #'var #'loc)
                 (quasisyntax* stx
                   (begin
                     (define loc (box #,e))
                     (define-syntax var
                       (syntax-id-rules (set!)
                         [(set! var val) (set-box! loc val)]
                         [(var . arg) ((unbox loc) . arg)]
                         [var (unbox loc)])))))]
              [(eq? e #'expr) stx]
              [else (quasisyntax* stx (define-values (var) #,e))]))]
     [(define-values (var ...) expr)
      (let ([e (bmv #'expr)]
            [vs (syntax->list #'(var ...))])
        (cond [(any-mutated? vs)
               (let ([locs (generate-temporaries vs)])
                 (for ([v (in-list vs)]
                       [loc (in-list locs)]
                       #:when (mutated? v))
                   (dict-set! varref-tbl v loc))
                 (quasisyntax* stx
                   (begin
                     (define-values #,locs #,e)
                     #,@(for/list ([v vs][loc locs] #:when (mutated? v))
                          #`(set! #,loc (box #,loc)))
                     #,@(for/list ([v vs][loc locs])
                          (if (mutated? v)
                              #`(define-syntax #,v
                                  (syntax-id-rules (set!)
                                    [(set! #,v val) (set-box! #,loc val)]
                                    [(#,v . arg) ((unbox #,loc) . arg)]
                                    [#,v (unbox #,loc)]))
                              #`(define-values (#,v) #,loc))))))]
              [(eq? e #'expr) stx]
              [else (quasisyntax* stx (define-values (var ...) #,e))]))]
     [(let-values ([(var ...) expr] ...) body ...)
      (let-values ([(pure-es? es) (bmv/list #'(expr ...))]
                   [(pure-fs? fs) (bmv/list #'(body ...))]
                   [(vs) (syntax->list #'(var ... ...))])
        (cond [(any-mutated? vs)
               (with-syntax ([(e ...) es])
                 (quasisyntax* stx
                   (let-values ([(var ...) e] ...)
                     #,@(for/list ([v vs] #:when (mutated? v))
                          #`(set! #,v (box #,v))) 
                     #,@fs)))]
              [(and pure-es? pure-fs?) stx]
              [else 
               (with-syntax ([(e ...) es])
                 (quasisyntax* stx
                   (let-values ([(var ...) e] ...)
                     #,@fs)))]))]
     [(letrec-values ([(var ...) expr] ...) body ...) 
      (let-values ([(pure-es? es) (bmv/list #'(expr ...))]
                   [(pure-fs? fs) (bmv/list #'(body ...))]
                   [(vs) (syntax->list #'(var ... ...))])
        (cond [(any-mutated? vs)
               (let ([ves (syntax->list #'((var ...) ...))])
                 (quasisyntax* stx
                   (letrec-values ([#,vs (apply values (make-list #,(length vs) undefined))])
                     #,@(for/list ([v vs] #:when (mutated? v))
                          #`(set! #,v (box #,v)))
                     #,@(for/fold ([result '()]) ([ve ves] [e es])
                          `(,@result 
                            ,#`(set!-values #,ve #,e)
                            ,@(for/list ([v (syntax->list ve)] #:when (mutated? v))
                                #`(set! #,v (box #,v)))))
                     #,@fs)))]
              [(and pure-es? pure-fs?) stx]
              [else 
               (with-syntax ([(e ...) es])
                 (quasisyntax* stx
                   (letrec-values ([(var ...) e] ...)
                     #,@fs)))]))]
     [(letrec-syntaxes+values stx-decls ([(var ...) expr] ...) body ...) 
      (let*-values ([(pure-es? es) (bmv/list #'(expr ...))]
                    [(pure-fs? fs) (bmv/list #'(body ...))]
                    [(vs) (syntax->list #'(var ... ...))])
        (cond [(any-mutated? vs)
               (let ([ves (syntax->list #'((var ...) ...))])
                 (quasisyntax* stx
                   (letrec-syntaxes+values stx-decls ([#,vs (apply values (make-list #,(length vs) undefined))])
                     #,@(for/list ([v vs] #:when (mutated? v))
                          #`(set! #,v (box #,v)))
                     #,@(for/fold ([result '()]) ([ve ves] [e es])  
                          `(,@result 
                            ,#`(set!-values #,ve #,e)
                            ,@(for/list ([v (syntax->list ve)] #:when (mutated? v))
                                #`(set! #,v (box #,v)))))
                     #,@fs)))]
              [(and pure-es? pure-fs?) stx]
              [else 
               (with-syntax ([(e ...) es])
                 (quasisyntax* stx
                   (letrec-syntaxes+values stx-decls ([(var ...) e] ...)
                     #,@fs)))]))]
     [(#%plain-lambda formals . rest)
      (let ([body (bmv/proc-body #'formals #'rest)])
        (cond [(eq? body #'rest) stx]
              [else (quasisyntax* stx (#%plain-lambda formals #,@body))]))]
     [(case-lambda . rest)
      (let* ([r (syntax->list #'rest)]
             [fs (for/list ([fb r])
                  (with-syntax ([(f . b) fb])
                    (let ([body (bmv/proc-body #'f #'b)])
                      (if (eq? body #'b)
                          fb 
                          (quasisyntax* fb (f #,@body))))))])
        (cond [(equal? r fs) stx]
              [else (quasisyntax* stx (case-lambda #,@fs))]))]
     [(if . rest) (bmv/rest stx #'if #'rest)]
     [(#%expression . rest) (bmv/rest stx #'#%expression #'rest)]
     [(#%plain-app . rest)  (bmv/rest stx #'#%plain-app #'rest)]
     [(begin . rest)  (bmv/rest stx #'begin #'rest)]
     [(begin0 . rest) (bmv/rest stx #'begin0 #'rest)]
     [(with-continuation-mark . rest) (bmv/rest stx #'with-continuation-mark #'rest)]
     [(#%plain-module-begin . rest) (quasisyntax* stx (#%module-begin #,@(map bmv (syntax->list #'rest))))]
     [(#%variable-reference x)
      #`(#%variable-reference
         #,(free-id-table-ref varref-tbl #'x (λ () #'x)))]
     [_ stx]))
  
    (bmv form))
