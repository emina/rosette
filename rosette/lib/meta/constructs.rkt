#lang s-exp rosette

(require "define.rkt" (for-syntax racket/syntax) "compile.rkt")

(provide define-synthax choose choose-number ?? option tag syntax/source)

(define-syntax (define-synthax stx)
  (syntax-case stx ()
    [(_ (id arg ...) #:assert guard body)
     (quasisyntax/loc stx 
       (define-synthesis-rule (id arg ...)
         #:guard    guard
         #:declare  [unroll? boolean?] 
         #:compile  (if unroll? body (assert #f))
         #:generate (lambda (gen sol)
                      (cond [(selected? unroll? sol)     (gen (syntax/source body))]
                            [(selected? (! unroll?) sol) (gen #'(assert #f))]
                            [else #f]))))]
    [(_ (id arg ...) body)
     (syntax/loc stx 
       (define-synthesis-rule (id arg ...)
         #:compile  body
         #:generate (lambda (gen sol) (gen (syntax/source body)))))]))

(define-synthesis-rule (choose t ... tn)
  #:declare  ([v boolean?] ... : #'(t ...))
  #:compile  (cond [v t] ... [else tn]);(wrap (cond [v t] ... [else tn]))
  #:generate (lambda (gen sol) 
               (cond [(selected? v sol) (gen (syntax/source t))] ...
                     [(selected? (! (|| v ...)) sol) (gen (syntax/source tn))]
                     [else #f])))

(define-synthesis-rule (choose-number low high)
  #:declare [v integer?]
  #:compile (begin (assert (<= low v))
                   (assert (<= v high))
                   v)
  #:generate (lambda (gen sol)
               (let ([val (evaluate v sol)])
                 (and (not (term? val))
                      (gen #`#,val)))))

(define-synthesis-rule (??)
  #:declare [v integer?]
  #:compile v
  #:generate (lambda (gen sol)
               (let ([val (evaluate v sol)])
                 (and (not (term? val))
                      (gen #`#,val)))))

(define-synthesis-rule (option p expr)
  #:declare  [opt? boolean?]
  #:compile  (let ([e (thunk expr)]) (if opt? (p (e)) (e)))
  #:generate (lambda (gen sol) 
               (cond [(selected? opt? sol) #`(#,(gen (syntax/source p)) #,(gen (syntax/source expr)))]
                     [(selected? (! opt?) sol) (gen (syntax/source expr))]
                     [else #f])))

(define (selected? v sol) 
  (let ([v (evaluate v sol)])
    (and (not (term? v)) v)))

(define-syntax (tag stx)
  (syntax-case stx ()
    [(tag [id] form)
     (with-syntax ([tagged-id (format-id #'tag "~a" #'id #:source #'tag #:props #'tag)])
       (syntax/loc stx
         (local [(define-synthesis-rule (tagged-id) #:compile form)]
           (tagged-id))))]
    [(id [] form)
     (syntax/loc stx
       (tag [id] form))]))


#|
(require "define.rkt" (for-syntax racket/syntax "compile.rkt" "identifiers.rkt") "compile.rkt")
(define-syntax (choose stx)
  (syntax-case stx ()
    [(choose t ... tn)
     (with-syntax ([(v ...) (generate-identifiers #'(t ...) #:source #'choose)])
       (synthesizer #:source   #'choose
                    #:declare  #'(([v boolean?] ...))
                    #:compile  (syntax/source (wrap (cond [v t] ... [else tn])))
                    #:generate (quasisyntax/loc stx (lambda (gen) 
                                                (cond [(selected? v) (gen (syntax/source t))] ...
                                                      [(selected? (! (|| v ...))) (gen (syntax/source tn))]
                                                      [else #f])))))]))|#
