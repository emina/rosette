#lang racket

(require (for-syntax racket/syntax)
         racket/stxparam racket/stxparam-exptime
         "../util/log.rkt" 
         "term.rkt" "bool.rkt")
 
(provide @assert pc with-asserts with-asserts-only relax 
         (rename-out [export-asserts asserts])
         clear-asserts)

(define (export-asserts) (remove-duplicates (asserts)))

(define (clear-asserts)  (asserts '()))
    

(define asserts 
  (make-parameter 
   '()
   (match-lambda [(? list? xs) xs]
                 [x (if (eq? x #t) (asserts) (cons x (asserts)))])))

(define pc 
  (make-parameter 
   #t
   (lambda (new-pc) 
     (or (boolean? new-pc) 
         (and (term? new-pc) (equal? @boolean? (term-type new-pc)))
         (error 'pc "expected a boolean path condition, given a ~s" (type-of new-pc)))
     (or (&& (pc) new-pc)
         (error 'pc "infeasible path condition")))))

(define-syntax (@assert stx)
  (syntax-case stx ()
    [(_ val)            (syntax/loc stx (@assert val #f #f))]
    [(_ val msg)        (syntax/loc stx (@assert val msg #f))]
    [(_ val msg origin) 
     (syntax/loc stx 
       (syntax-parameterize ([relax (syntax-rules () [(_ form loc) form])])
                            (let ([guard (not-false? val)])
                              (asserts (term-track-origin (=> (pc) guard) origin))
                              (when (false? guard)
                                (raise-assertion-error msg origin)))))]))

(define-syntax-parameter relax
  (lambda (stx)
    (syntax-case stx () [(_ form origin) #'form])))

(define (not-false? v)
  (or (eq? v #t) (! (@false? v))))

(define (raise-assertion-error msg origin)
  (if (procedure? msg)
      (msg)
      (error 'assert (cond [(and msg origin) (format "~a\n  failure origin: ~a" msg origin)]
                           [msg (format "~a" msg)]
                           [origin (format "failed at ~a" origin)]
                           [else "failed"]))))
                     
(define-syntax (with-asserts stx)
  (syntax-case stx (begin)
    [(_ (begin form ...)) #'(with-asserts (let () form ...))]
    [(_ form) #`(parameterize ([asserts (asserts)])
                  (let*-values ([(val cpu real gc) (time-apply (lambda () form) '())]  
                                [(asserts) (remove-duplicates (asserts))])
                    (log-symbolic-execution-stats asserts cpu real gc)
                    (values (car val) asserts)))]))

(define-syntax-rule (with-asserts-only form)
  (let-values ([(out asserts) (with-asserts form)])
    asserts))

(define (log-symbolic-execution-stats asserts cpu real gc)
  (unless (null? asserts)
    (log-info ['rosette] "symbolic execution time (ms): cpu = ~s, real = ~s, gc = ~s" 
              cpu real gc)))
  
