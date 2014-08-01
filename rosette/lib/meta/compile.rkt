#lang racket

(require racket/stxparam  
         (for-syntax racket/stxparam syntax/template)
         "../util/syntax-properties.rkt"
         (only-in rosette define-symbolic)
         (only-in "identifiers.rkt" syntax-identifier)
         (only-in "generate.rkt" add-generator!))

(provide  (for-syntax synthesizer) syntax/source in-context (rename-out [the-context context]))

(define-for-syntax (synthesizer 
                    #:source source      ; source syntax
                    #:declare var-decls  ; syntax-list of variable-name/type pairs: #'{([v type] ...) ...}
                    #:compile compile    ; syntax for the synthesizer body
                    #:generate gen)      ; syntax for the code generator 
  ;(printf "source/compile ~a ~a\n" source (syntax->list compile))
  (with-syntax ([{([id type] ...) ...} var-decls])
    (quasisyntax/loc source
      (local [(define-symbolic id type) ... ...]
        (in-context (syntax/source #,source) 
                    (begin0 #,compile
                            (add-generator! (context) #,gen)))))))

(define-syntax-parameter context (syntax-id-rules () [_ '()]))

(define-syntax-rule (the-context) context)

(define-syntax (in-context stx)
  (syntax-case stx ()
    [(_ loc template)
     (quasisyntax/loc stx 
       (let ([extended (cons (syntax-identifier loc) context)])
         (syntax-parameterize ([context (syntax-id-rules () [_ extended])])
                              template)))]))



