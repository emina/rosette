#lang racket

(require (for-syntax racket "compile.rkt" "identifiers.rkt" rosette/base/core/assert)
         "compile.rkt" "identifiers.rkt" rosette/base/core/assert)

(provide define-synthesis-rule procedure->syntax)
    
(define-syntax (define-synthesis-rule stx)
  (syntax-case stx (:)
    [(_ (id rest ...) 
        #:guard guard
        #:declare [[var type] ellipses ... : from] ... 
        #:compile compile 
        #:generate generate) 
     (quasisyntax/loc stx
       (define-syntax (id stx)
         (syntax-case stx ()
           [(call rest ...) 
            (cond [(eval #'guard)
                   (with-syntax ([(var ellipses ...) (generate-identifiers from #:source #'call)] ...)                     
                     (synthesizer #:source   #'call
                                  #:declare  #'(([var type] ellipses ...) ...)
                                  #:compile  (syntax/source compile)
                                  #:generate (syntax/source generate)))]
                  [else (syntax/loc stx (@assert #f))])])))]
    [(_ (id rest ...) #:guard guard #:declare [var type] ... #:compile compile #:generate generate)
     (syntax/loc stx 
       (define-synthesis-rule (id rest ...) 
         #:guard guard 
         #:declare ([var type] : #'(var)) ... 
         #:compile compile 
         #:generate generate))]
    [(_ (id rest ...) #:declare decl ... #:compile compile  #:generate generate)
     (syntax/loc stx 
       (define-synthesis-rule (id rest ...)
         #:guard #t
         #:declare decl ... 
         #:compile compile 
         #:generate generate))]
    [(_ (id rest ...) #:compile compile #:generate generate)
     (syntax/loc stx 
       (define-synthesis-rule (id rest ...) 
         #:guard #t
         #:declare 
         #:compile compile 
         #:generate generate))]
    [(_ (id rest ...) #:compile compile)
     (syntax/loc stx 
       (define-synthesis-rule (id rest ...) 
         #:guard #t
         #:declare 
         #:compile compile
         #:generate (lambda (gen) (gen #'compile))))]))

(define-syntax-rule (procedure->syntax id) (lambda any #`(id #,@any?)))



#|(quasisyntax/loc stx
    (local [(define-symbolic var type) ... ellipses ... ...]
       (in-context #'call
         (begin0 compile
             (add-generator! (context) generate))))))]|#
