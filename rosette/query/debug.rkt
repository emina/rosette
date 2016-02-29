#lang racket

(require (for-syntax racket/syntax) racket/stxparam
         (only-in racket/syntax format-id)
         slideshow/code (only-in slideshow vl-append current-font-size)
         (only-in "core.rkt" current-solver ∃-debug eval/asserts)
         "../lib/util/syntax-properties.rkt" "../lib/util/syntax.rkt"
         (only-in "../base/form/app.rkt" app)
         (only-in "../base/core/reflect.rkt" symbolics)
         (only-in "../solver/solution.rkt" unsat? core)
         "../base/core/bool.rkt"  "../base/form/state.rkt" 
         "../base/core/equality.rkt" "../base/core/term.rkt")

(provide relax? relate debug define/debug protect assert true false render)

(define-syntax debug
  (syntax-rules ()
    [(debug form) 
     (parameterize ([current-oracle (oracle)])
       (∃-debug (eval/asserts (thunk form))))]
    [(debug [pred other ...] form)
     (parameterize ([relax? (list pred other ...)])
       (debug form))]))

(define-syntax-rule (define/debug head body ...) 
  (define head
    (syntax-parameterize
     ([app app-track])
     body ...)))

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ expr) (syntax/loc stx (@assert (protect expr) #f))]
    [(_ expr msg) (syntax/loc stx (@assert (protect expr) (protect msg)))]))


(define-for-syntax (app-track stx)
  (syntax-case stx ()
    [(_ proc arg ...) 
     (quasisyntax/loc stx
       (call-with-values (thunk (#%app proc arg ...))
                         (relax-values (syntax/source proc))))]))

(define-syntax-rule (protect expr)
  (syntax-parameterize ([app (syntax-rules () [(_ proc arg (... ...)) (#%app proc arg (... ...))])])
                       expr))

(define-syntax true
  (syntax-id-rules (set!)
    [(set! true e) (error 'set! "cannot reassign the constant true")]
    [true (relax #t true)]))

(define-syntax false
  (syntax-id-rules (set!)
    [(set! false e) (error 'set! "cannot reassign the constant false")]
    [false (relax #f false)]))

(define relax?
  (make-parameter none/c
                  (lambda (pred)
                    (unless (or (type? pred) (and (list? pred) (andmap type? pred)))
                      (error 'relax? "expected a type or list of types, given ~s" pred))
                    (match pred
                      [(or (? type? p) (list p)) (lambda (v) (eq? (type-of v) p))]
                      [_ (lambda (v) (not (false? (memq (type-of v) pred))))]))))

(define relate
  (make-parameter @equal?
                  (lambda (rel)
                    (unless (and (procedure? rel) (procedure-arity-includes? rel 2))
                      (error 'relate "expected a 2 argument procedure, given ~s" rel))
                    rel)))

(define (relax-values origin)
  (lambda rs 
    (apply values 
           (map (lambda (r) 
                  (if (and ((relax?) r) (not (relaxer? r)))
                      (let ([tracked (constant (list relaxer origin) (type-of r))])
                        (@assert ((relate) tracked r))
                        tracked)
                      r))
                rs))))

(define relaxer #'relaxer)

(define (relaxer? val)
  (match val
    [(constant (list (== relaxer) _) _) #t]
    [_ #f]))

(define (debug-origin val)
  (match val
    [(constant (list (== relaxer) origin) _) origin]
    [_ #f]))

(define (render sol [font-size 16])
  (unless (unsat? sol)
    (error 'render "expected an unsatisfiable solution, given ~a" sol))
  (let* ([core (filter-map location (remove-duplicates (filter-map debug-origin (symbolics (core sol)))))]
         [debugged (debugged-forms (remove-duplicates (map location-source core)) core)]
         [core (apply set core)])
    (parameterize ([code-colorize-enabled #t]
                   [current-font-size font-size])
      (apply vl-append (for/list ([stx debugged])
                         (eval #`(code #,(colorize-code stx core))))))))

(define (debugged-forms paths core)
  (define (debugged? loc)
    (for/or ([core-loc core])
      (location-contains? loc core-loc)))
  (apply 
   append
   (for/list ([source (map read-module paths)])
     (syntax-case source () 
       [(mod id lang (mod-begin forms ...))
        (for/list ([form (syntax->list #'(forms ...))]
                   #:when (debugged? (location form)))
          form)]
       [_ (error 'debugged-forms "expected a module, given ~a" source)]))))
  

(define (colorize-code stx core)
  (syntax-case stx ()
    [(id rest ...)
     (and (identifier? #'id) (free-label-identifier=? #'id #'quote))
     stx]
    [(expr rest ...)
     (let* ([children (for/list ([e (syntax->list stx)]) (colorize-code e core))]
            [children (datum->syntax stx children stx)]
            [color (if (or (set-member? core (location #'expr)) 
                           (set-member? core (location stx))) 
                       "red" "gray")])
       (quasisyntax/loc stx (colorize #,color #,children)))]
    [_ (let ([leaf (if (identifier? stx) (reformat-identifier stx) stx)])
         (if (set-member? core (location stx)) 
             (quasisyntax/loc stx (colorize "red" #,leaf))
             leaf))]))
         

(define-syntax colorize 
  (make-code-transformer 
   (syntax-rules () 
     [(_ color e) 
      (parameterize ([code-colorize-enabled #t]
                     [current-base-color color]
                     [current-comment-color color]
                     [current-keyword-color color]
                     [current-id-color color]
                     [current-literal-color color]
                     [current-const-color color])
        (code e))])))

(define (reformat-identifier stx)
  (format-id stx "~a" 
             (string->symbol (~s (syntax->datum stx)))
             #:source stx
             #:props stx))


