#lang racket

(require "../solver.rkt" "../solution.rkt" 
         "../common/server.rkt" (only-in "../common/util.rkt" filter-asserts)
         "../../config/log.rkt" "cmd.rkt" (rename-in "env.rkt" [env make-env]))

(provide smt%)

(define smt%
  (class* object% (solver<%>) (inspect (make-inspector))
    [init path]
    [init opts]
    
    (define server 
      (new server% 
           [initializer (thunk (apply subprocess #f #f #f path opts))]
           [stderr-handler (lambda (err)
                             (let ([expr (read err)])
                               (unless (eof-object? expr)
                                 (log-error [this] "~a" expr))))]))
    
    (define asserts '())
    (define env (make-env))
    
    (super-new)
    
    (define/public assert 
      (lambda in (set! asserts (append asserts (filter-asserts in)))))
    
    (define/public (clear)
      (send server write clear-solver)
      (set!-values (asserts env) (values '() (make-env))))
    
    (define/public (shutdown)
      (clear)
      (send server shutdown))
    
    (define/public (debug)     (error 'debug "not supported by ~a" this))      
    (define/public (solve-all) (error 'solve-all "not supported by ~a" this))
    
    (define/public (solve)
      (set! asserts (remove-duplicates asserts))
      (cond [(ormap false? asserts) (unsat)]
            [else    
             (parameterize ([current-log-source this])
               (log-time [this] "compilation" : 
                         (send server write (curry encode env asserts))
                         (set! asserts '()))
               (log-time [this] "solving"     : 
                         (send server read (curry decode env))))]))))




