#lang racket

(require racket/runtime-path "../solver.rkt" "../solution.rkt" 
         "../common/server.rkt" (only-in "../common/util.rkt" filter-asserts)
         "../../config/log.rkt" "cmd.rkt" (rename-in "env.rkt" [env make-env]))

(provide z3%)

(define-runtime-path z3 (build-path ".." ".." ".." "bin" "z3"))

(define z3%
  (class* object% (solver<%> writable<%>)

    (define z3-server (new server% 
                           [initializer (thunk (subprocess #f #f #f z3 "-smt2" "-in"))]
                           [stderr-handler (lambda (err)
                                             (let ([expr (read err)])
                                               (unless (eof-object? expr)
                                                 (log-error ["z3"] "~a" expr))))]))
    
    (define asserts '())
    (define env (make-env))
    
    (super-new)
    
    (define/public (custom-write port) (fprintf port "z3"))
    (define/public (custom-display port) (custom-write port))
    
    (define/public assert 
      (lambda in (set! asserts (append asserts (filter-asserts in)))))
    
    (define/public (clear) 
      (send z3-server shutdown)
      (set!-values (asserts env) (values '() (make-env))))
    
    (define/public (shutdown)  (clear))
    
    (define/public (debug)     (error 'debug "not supported by z3%"))      
    (define/public (solve-all) (error 'solve-all "not supported by z3%"))
    
    (define/public (solve)
      (set! asserts (remove-duplicates asserts))
      (cond [(ormap false? asserts) (unsat)]
            [else    
             (parameterize ([current-log-source this])
               (log-time [this] "compilation" : 
                         (send z3-server write (curry encode env asserts))
                         (set! asserts '()))
               (log-time [this] "solving"     : 
                         (send z3-server read (curry decode env))))]))))




