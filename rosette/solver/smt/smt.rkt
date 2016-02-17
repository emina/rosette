#lang racket

(require "../solver.rkt" "../solution.rkt" 
         "server.rkt"  "../../base/util/log.rkt" "cmd.rkt" 
         (only-in "../../base/core/term.rkt" term? term-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (rename-in "env.rkt" [env make-env]))

(provide smt%)

(define (filter-asserts asserts)
  (for/list ([a asserts] #:unless (equal? a #t))
    (unless (or (boolean? a) (and (term? a) (equal? @boolean? (term-type a))))
      (error 'assert "expected a boolean value, given ~s" a))
    a))

(define smt%
  (class* object% (solver<%>) (inspect (make-inspector))
    [init path]
    [init opts]
    
    (define smt-server (server path opts)) 
    
    (define asserts '())
    (define env (make-env))
    
    (super-new)
    
    (define/public assert 
      (lambda in (set! asserts (append asserts (filter-asserts in)))))
    
    (define/public (clear)
      (server-write smt-server clear-solver)
      (set!-values (asserts env) (values '() (make-env))))
    
    (define/public (shutdown)
      (clear)
      (server-shutdown smt-server))
    
    (define/public (debug)     (error 'debug "not supported by ~a" this))      
    (define/public (solve-all) (error 'solve-all "not supported by ~a" this))
    
    (define/public (solve)
      (set! asserts (remove-duplicates asserts))
      (cond [(ormap false? asserts) (unsat)]
            [else    
             (parameterize ([current-log-source this])
               (log-time [this] "compilation" : 
                         (server-write smt-server (curry encode env asserts))
                         (set! asserts '()))
               (log-time [this] "solving"     : 
                         (server-read smt-server (curry decode env))))]))))




