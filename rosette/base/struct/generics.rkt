#lang racket

(require (for-syntax (only-in racket/syntax
                              format-id wrong-syntax generate-temporary
                              current-syntax-context)
                     (only-in syntax/stx stx-pair? stx-car stx-cdr))
         (only-in racket/generic define-generics)
         (only-in "../core/forall.rkt" for/all)
         "../core/union.rkt")

(provide @define-generics @make-struct-type-property)

(begin-for-syntax

  ;; parse is copied from racket/generic
  ;; One modification (marked below): If the #:defined-predicate option
  ;; is not present, it returns #f instead of (generate-temporary)
  (define (parse stx [options (hasheq)])
    (syntax-case stx ()
      [(#:defined-predicate name . args)
       (identifier? #'name)
       (if (hash-ref options 'support #f)
           (wrong-syntax (stx-car stx)
                         "duplicate #:defined-predicate specification")
           (parse #'args (hash-set options 'support #'name)))]
      [(#:defined-predicate . other)
       (wrong-syntax (stx-car stx) "invalid #:defined-predicate specification")]
      [(#:defined-table name . args)
       (identifier? #'name)
       (if (hash-ref options 'table #f)
           (wrong-syntax (stx-car stx)
                         "duplicate #:defined-table specification")
           (parse #'args (hash-set options 'table #'name)))]
      [(#:defined-table . other)
       (wrong-syntax (stx-car stx) "invalid #:defined-table specification")]
      [(#:defaults (clause ...) . args)
       (if (hash-ref options 'defaults #f)
           (wrong-syntax (stx-car stx) "duplicate #:defaults specification")
           (let loop ([defaults '()]
                      [clauses (reverse (syntax->list #'(clause ...)))])
             (if (pair? clauses)
                 (syntax-case (car clauses) ()
                   [(pred #:dispatch disp defn ...)
                    (loop (cons #'[pred disp defn ...] defaults)
                          (cdr clauses))]
                   [(pred defn ...)
                    (with-syntax ([name (generate-temporary #'pred)])
                      (loop (cons #'[pred #:same defn ...] defaults)
                            (cdr clauses)))]
                   [clause
                    (wrong-syntax #'clause "invalid #:defaults specification")])
                 (parse #'args
                        (hash-set* options 'defaults defaults)))))]
      [(#:defaults . other)
       (wrong-syntax (stx-car stx) "invalid #:defaults specification")]
      [(#:fast-defaults (clause ...) . args)
       (if (hash-ref options 'fast-defaults #f)
           (wrong-syntax (stx-car stx)
                         "duplicate #:fast-defaults specification")
           (let loop ([fast-defaults '()]
                      [clauses (reverse (syntax->list #'(clause ...)))])
             (if (pair? clauses)
                 (syntax-case (car clauses) ()
                   [(pred #:dispatch disp defn ...)
                    (loop (cons #'[pred disp defn ...] fast-defaults)
                          (cdr clauses))]
                   [(pred defn ...)
                    (with-syntax ([name (generate-temporary #'pred)])
                      (loop (cons #'[pred #:same defn ...] fast-defaults)
                            (cdr clauses)))]
                   [clause
                    (wrong-syntax #'clause
                                  "invalid #:fast-defaults specification")])
                 (parse #'args
                        (hash-set* options
                                   'fast-defaults fast-defaults)))))]
      [(#:fast-defaults . other)
       (wrong-syntax (stx-car stx) "invalid #:fast-defaults specification")]
      [(#:fallbacks [fallback ...] . args)
       (if (hash-ref options 'fallbacks #f)
           (wrong-syntax (stx-car stx) "duplicate #:fallbacks specification")
           (parse #'args (hash-set options 'fallbacks #'[fallback ...])))]
      [(#:fallbacks . other)
       (wrong-syntax (stx-car stx) "invalid #:fallbacks specification")]
      [(#:derive-property prop impl . args)
       (parse #'args
              (hash-set options
                        'derived
                        (cons (list #'prop #'impl)
                              (hash-ref options 'derived '()))))]
      [(#:derive-property . other)
       (wrong-syntax (stx-car stx) "invalid #:derive-property specification")]
      [(kw . args)
       (keyword? (syntax-e #'kw))
       (wrong-syntax #'kw "invalid keyword argument")]
      [((_ . _) . args)
       (if (hash-ref options 'methods #f)
           (wrong-syntax (stx-car stx) "duplicate methods list specification")
           (let loop ([methods (list (stx-car stx))] [stx #'args])
             (syntax-case stx ()
               [((_ . _) . args) (loop (cons (stx-car stx) methods) #'args)]
               [_ (parse stx (hash-set options 'methods (reverse methods)))])))]
      [(other . args)
       (wrong-syntax #'other
                     "expected a method identifier with formal arguments")]
      [() (values (hash-ref options 'methods '())
                  ;; MODIFICATION: Third argument to hash-ref changed
                  ;; from generate-temporary to #f
                  (hash-ref options 'support #f)
                  (hash-ref options 'table #f)
                  (hash-ref options 'fast-defaults '())
                  (hash-ref options 'defaults '())
                  (hash-ref options 'fallbacks '())
                  (hash-ref options 'derived '()))]
      [other
       (wrong-syntax #'other
                     "expected a list of arguments with no dotted tail")]))

  (define (index-of name-stx formals-stx)
    (let loop ([i 0] [formals formals-stx])
      (unless (stx-pair? formals)
        (wrong-syntax
         formals-stx
         "did not find the generic name ~a among the required, by-position arguments"
         (syntax->datum name-stx)))
      (define c (stx-car formals))
      (cond [(identifier? c)
             (if (free-identifier=? name-stx (stx-car formals))
                 (datum->syntax name-stx i)
                 (loop (+ i 1) (stx-cdr formals)))]
            [(keyword? (syntax->datum c)) ; count only by-position required arguments
             (loop i (stx-cdr (stx-cdr formals)))]
            [else
             (wrong-syntax c "required arguments must precede optional arguments")]))))

(define-syntax (@define-generics stx)
  (syntax-case stx ()
    [(_ id . rest)
     (parameterize ([current-syntax-context stx])
       (unless (identifier? #'id)
         (wrong-syntax #'id "expected an identifier"))
       (define-values
         (methods support table fasts defaults fallbacks derived)
         (parse #'rest))
       
       (when table
         (wrong-syntax table
                       "#:defined-table option is not supported in Rosette"))

       (with-syntax ([id? (format-id #'id "~a?" #'id #:source #'id)]
                     [((method-name . method-args) ...) methods]
                     [support-name support])
         (with-syntax ([(method-index ...)
                        (map (lambda (args) (index-of #'id args))
                             (syntax-e #'(method-args ...)))])
           (syntax/loc stx 
             (begin
               (define-generics id . rest)
               (lift-if-exists id? 0)
               (lift-if-exists support-name 0)
               (lift-if-exists method-name method-index) ...)))))]))
    
(define (@make-struct-type-property name [guard #f] [supers null] [can-impersonate? #f])
  (define-values (prop:p p? p-ref) 
    (make-struct-type-property name guard supers can-impersonate?))
  (values prop:p (lift p? 0) (lift p-ref 0)))

(define-syntax (lift-if-exists stx)
  (syntax-case stx ()
    [(_ proc receiver-index)
     (if (syntax->datum #'proc)
         (syntax/loc stx
           (set! proc (lift proc receiver-index)))
         (syntax/loc stx
           (void)))]))

(define (lift proc receiver-index)
  (define-values (required-kws allowed-kws) (procedure-keywords proc))
  (define arity (procedure-arity proc))
  (procedure-rename
   (if (null? allowed-kws)
       (procedure-reduce-arity
        (lambda args
          (define receiver (list-ref args receiver-index))
          (if (union? receiver)
              (for/all ([r receiver])
                (apply proc (list-set args receiver-index r)))
              (apply proc args)))
        arity)
       (procedure-reduce-keyword-arity
        (make-keyword-procedure
         (lambda (kws kw-args . args)
           (define receiver (list-ref args receiver-index))
           (if (union? receiver)
               (for/all ([r receiver])
                 (keyword-apply proc kws kw-args (list-set args receiver-index r)))
               (keyword-apply proc kws kw-args args))))
        arity
        required-kws
        allowed-kws))
   (or (object-name proc) 'lifted)))

#|
; sanity check
(@define-generics foo [some foo])
some

(struct bar (arg)
  #:methods gen:foo
  [(define (some self) (bar-arg self))])

(some (bar 'yes))

(require (only-in rosette/base/form/define define-symbolic)
         (only-in rosette/base/core/bool @boolean?)
         (only-in rosette/base/core/real @* @+))

(define-symbolic b @boolean?)
(some (@if b (bar 'yes) (bar 'no)))
(foo? (@if b (bar 'yes) (bar 'no)))

(@define-generics xyzzy
  (h xyzzy))

(@define-generics variadic
  (f variadic . x)
  (g variadic))

(struct multiplier (y) #:transparent
  #:methods gen:variadic
  [(define (f self . x) (foldl @* (multiplier-y self) x))
   (define (g self) 'g-mult)]
  #:methods gen:xyzzy
  [(define (h self) 42)])

(struct adder (z) #:transparent
  #:methods gen:variadic
  [(define (f self . x) (foldl @+ (adder-z self) x))
   (define (g self) 'g-add)])

(define thing (@if b (multiplier 3) (adder 3)))
(variadic? thing)
thing
(f thing 2 5)
(g thing)
(h thing)|#
