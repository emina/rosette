#lang racket

(require racket/syntax "../base/type.rkt")

(provide current-log-source current-log-handler
         define-log log-handler 
         log-fatal log-error log-warning log-info log-debug
         log-time)

(define current-log-source (make-parameter #f))
    
(define (log-handler  #:name    [name 'log-handler]
                      #:fatal   [log-fatal? any/c] 
                      #:error   [log-error? any/c] 
                      #:warning [log-warning? any/c]
                      #:info    [log-info? none/c] 
                      #:debug   [log-debug? none/c]
                      #:logger  [logger (current-logger)])
  
  (unless (logger? logger)
    (error 'log-handler "expected a logger, given ~s" logger))
  
  (for ([level      (in-list '(#:debug #:info #:warning #:error #:fatal))]
        [log-level? (in-list `(,log-debug? ,log-info? ,log-warning? ,log-error? ,log-fatal?))]
        #:unless (and (procedure? log-level?) (flat-contract? log-level?)))
    (error 'log-handler "expected a flat contract for ~s, given ~s" level log-level?))
  
  (procedure-rename 
   (or 
    (for/first ([level      (in-list '(debug info warning error fatal))]
                [log-level? (in-list `(,log-debug? ,log-info? ,log-warning? ,log-error? ,log-fatal?))]
                #:unless (equal? log-level? none/c))
      (log-handler-procedure
       #:receiver (make-log-receiver logger level)
       #:fatal log-fatal? 
       #:error log-error? 
       #:warning log-warning? 
       #:info log-info? 
       #:debug log-debug?))
      void)
   name))
 
(define (log-handler-procedure #:receiver receiver
                               #:fatal log-fatal? 
                               #:error log-error? 
                               #:warning log-warning? 
                               #:info log-info? 
                               #:debug log-debug?)
  (lambda ()
    (let loop ()
      (match (sync receiver)
        [(vector lvl msg (or (? string? src) (? symbol? src) (? object? src) ) _ ...)
         (when (case lvl
                 [(fatal)   (or (log-fatal? src) (log-error? src) (log-warning? src) (log-info? src) (log-debug? src))]
                 [(error)   (or (log-error? src) (log-warning? src) (log-info? src) (log-debug? src))]
                 [(warning) (or (log-warning? src) (log-info? src) (log-debug? src))]
                 [(info)    (or (log-info? src) (log-debug? src))]
                 [(debug)   (log-debug? src)])
           (if src
               (printf "[~a] [~a] ~a\n" lvl src msg)
               (printf "[~a] ~a\n" lvl msg)))]
        [_ (void)])
      (loop))))

(define current-log-handler
  (let ([lh (thread (log-handler))])
    (case-lambda [() lh]
                 [(nh) (unless (and (procedure? nh) (procedure-arity-includes? nh 0))
                         (error 'current-log-handler
                                "expected a log handler, given ~s" nh))
                       (kill-thread lh)
                       (set! lh (thread nh))])))  
       
(define-syntax (log stx)
  (syntax-case stx ()
    [(_ [src] level msg ...) #'(let ([logger (current-logger)])
                                 (when (log-level? logger level)
                                   (log-message logger level (format msg ...) src)))]
    [(_ level msg ...)       #'(log [(current-log-source)] level msg ...)]))

(define-syntax (define-log stx)
  (syntax-case stx ()
    [(_ id #:source src #:level level) #'(define-syntax-rule (id msg (... ...))
                                           (log [src] level msg (... ...)))]
    [(_ id #:level level #:source src) #'(define-log id #:source src #:level level)]
    [(_ id #:source src)               #'(define-syntax-rule (id level msg (... ...))
                                           (log [src] level msg (... ...)))]
    [(_ id #:level level)              #'(define-syntax id
                                           (syntax-rules (id)
                                             [(id [src] msg (... ...))
                                              (log [src] level msg (... ...))]
                                             [(id msg (... ...))
                                              (log level msg (... ...))]))]))

(define-log log-fatal   #:level 'fatal)
(define-log log-error   #:level 'error)
(define-log log-warning #:level 'warning)
(define-log log-info    #:level 'info)
(define-log log-debug   #:level 'debug)


; Logs an info message about time-apply-style timing information for the evaluation of 
; expr. The result is the result of the last body.  If the desc value is provided, it 
; is used as the first word of the timing message; e.g., if "solving" is the descriptor,
; then then logged message will read "solving time (ms): cpu=..., real=..., gc=..."  The
; default descriptor is "evaluation."
(define-syntax log-time
  (syntax-rules (:)
    [(_ [src] desc : expr ...)
     (parameterize ([current-log-source src])
       (log-time desc : expr ...))]
     #|(let-values ([(out cpu real gc) (time-apply (thunk expr ...) '())])
       (log-info [src] "~a time (ms): cpu=~a, real=~a, gc=~a" desc cpu real gc)
       (apply values out))]|#
    [(_ desc : expr ...) 
     (let-values ([(out cpu real gc) (time-apply (thunk expr ...) '())])
       (log-info "~a time (ms): cpu=~a, real=~a, gc=~a" desc cpu real gc)
       (apply values out))]
    [(_ expr ...)
     (log-time "evaluation" : expr ...)]))



(define (location-info stx)
  (format "~a:~a:~a" (syntax-source stx) (syntax-line stx) (syntax-column stx)))

