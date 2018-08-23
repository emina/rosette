#lang racket

(require racket/splicing (for-syntax racket/syntax) racket/stxparam
         (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
         (only-in "merge.rkt" merge* unsafe-merge*)
         (only-in "union.rkt" union)
         (only-in "type.rkt" type-cast)
         "safe.rkt")

(provide define/lift (for-syntax lift-id) merge+ merge** unsafe-merge** flat-pattern-contract
         with@ drop@ add@)
   
(define (with@ name) 
  (and (regexp-match? #rx"^@.+$" name) name))

(define (drop@ name) 
  (if (regexp-match? #rx"^@.+$" name)
      (regexp-replace #rx"@" name "")
      name))

(define (add@ name) 
  (if (regexp-match? #rx"^@.+$" name)
      name
      (string-append "@" name)))

(define-syntax distribute
  (syntax-rules (_)
    [(distribute variant ps (proc arg ... _)) 
     (apply variant (for/list ([p ps]) (cons (car p) (proc arg ... (cdr p)))))]
    [(distribute variant ps (proc _ arg ...)) 
     (apply variant (for/list ([p ps]) (cons (car p) (proc (cdr p) arg ...))))]
    [(distribute variant ps proc) 
     (apply variant (for/list ([p ps]) (cons (car p) (proc (cdr p)))))]))

(define-syntax merge** 
  (syntax-rules (_)
    [(merge** ps (proc arg ... _)) (distribute merge* ps (proc arg ... _))]
    [(merge** ps (proc _ arg ...)) (distribute merge* ps (proc _ arg ...))]
    [(merge** ps proc)             (distribute merge* ps proc)])) 

(define-syntax unsafe-merge** 
  (syntax-rules (_)
    [(unsafe-merge** ps (proc arg ... _)) (distribute unsafe-merge* ps (proc arg ... _))]
    [(unsafe-merge** ps (proc _ arg ...)) (distribute unsafe-merge* ps (proc _ arg ...))]
    [(unsafe-merge** ps proc)             (distribute unsafe-merge* ps proc)])) 

(define-syntax merge+ 
  (syntax-rules ()
    [(_ expr #:error err) (apply merge* (assert-some expr err))]
    [(_ expr #:unless size #:error err) (apply merge* (assert-some expr #:unless size err))]))

(define-syntax (define/lift stx)
  (syntax-case stx (: :: ->)
    [(_ (id0 id ...) :: contracted? -> rosette-type?)
     (or (identifier? #'contracted?) (raise-argument-error "identifier?" #'contracted?))
     #'(begin 
         (define/lift id0 :: contracted? -> rosette-type?)
         (define/lift id  :: contracted? -> rosette-type?) ...)]
    [(_ id :: contracted? -> rosette-type?) ; repeated from (_ id : contracted? -> rosette-type?) - params don't work
     (or (identifier? #'contracted?) (raise-argument-error "identifier?" #'contracted?))
     #`(define (#,(lift-id #'id) val)
         (if (contracted? val) 
             (id val)
             (match (type-cast rosette-type? val (quote id))
               [(? contracted? v) (id v)]
               [(union vs) (apply merge* (assert-some 
                                      (for/list ([v vs] #:when (contracted? (cdr v))) 
                                        (cons (car v) (id (cdr v))))
                                      (contract-error (quote id) contracted? val)))]
               [_ (assert #f (contract-error (quote id) contracted? val))])))]
    [(_ (id0 id ...) : racket-contract? -> rosette-type?)
     #'(splicing-let ([contracted? racket-contract?]) 
         (define/lift id0 : contracted? -> rosette-type?)
         (define/lift id  : contracted? -> rosette-type?) ...)]
    [(_ id : contracted? -> rosette-type?)
     (identifier? #'contracted?)
     #`(define (#,(lift-id #'id) val)
         (if (contracted? val) 
             (id val)
             (match (type-cast rosette-type? val (quote id))
               [(? contracted? v) (id v)]
               [(union vs) (apply merge* (assert-some 
                                      (for/list ([v vs] #:when (contracted? (cdr v))) 
                                        (cons (car v) (id (cdr v))))
                                      (contract-error (quote id) contracted? val)))]
               [_ (assert #f (contract-error (quote id) contracted? val))])))]
    [(_ id : racket-contract? -> rosette-type?)  
     #`(splicing-let ([contracted? racket-contract?])
         (define/lift id : contracted? -> rosette-type?))]))

(define-for-syntax (lift-id id)
  (format-id id "@~a" (syntax-e id) #:source id #:props id))

(define-syntax-rule (flat-pattern-contract pattern)
  (flat-named-contract (quote pattern)	 	 	 	 
                       (match-lambda [pattern #t]
                                     [_ #f])))

