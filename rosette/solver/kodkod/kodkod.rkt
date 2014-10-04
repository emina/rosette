#lang racket

(require racket/runtime-path "../solver.rkt" "../solution.rkt" 
         (only-in "../common/util.rkt" filter-asserts)
         "../../config/log.rkt" (only-in "../../base/num.rkt" current-bitwidth)
         (only-in "kks.rkt" cmd TRUE FALSE) (prefix-in kks/ (only-in "kks.rkt" clear))
         "server.rkt" "cmd.rkt" (rename-in "env.rkt" [env make-env]))

(provide kodkod% kodkod-incremental%)

(define kodkod% 
  (class* object% (solver<%> writable<%>) (inspect (make-inspector))
    
    (define kodkod-server (new server% 
                               [initializer (thunk (kodkod-initializer #f))]
                               [stderr-handler (curry kodkod-stderr-handler this)]))
    (define asserts '())
    
    (super-new)
    
    (define/public (custom-write port)   (fprintf port "kodkod"))    
    (define/public (custom-display port) (custom-write port))
    
    (define/public (shutdown)  (send kodkod-server shutdown))
    (define/public (clear)     (set! asserts '()))
    
    (define/public assert 
      (lambda in (set! asserts (append asserts (filter-asserts in)))))
    
    (define/public (solve-all) (error 'solve-all "not supported by kodkod%"))    
    (define/public (solve)     (compile/solve FALSE 'Glucose))
    
    (define/public (debug)     
      (set! asserts (reverse asserts))
      (compile/solve TRUE 'MiniSatProver))
    
    (define (compile/solve cores? solver)
      (set! asserts (remove-duplicates asserts))
      (cond [(null? asserts) (sat (hash))]
            [(ormap false? asserts) (unsat '(#f))]
            [else
             (define env (make-env))
             (log-time [this] "compilation" : (send kodkod-server write (compile env asserts cores? solver)))
             (log-time [this] "solving"     : (send kodkod-server read  (curryr decode env)))]))))

(define kodkod-incremental% 
  (class* object% (solver<%> writable<%>) 
    
    (define kodkod-server (new server% 
                               [initializer (thunk (kodkod-initializer #t))]
                               [stderr-handler (curry kodkod-stderr-handler this)]))
    (define asserts '())
    (define env #f)
    
    (super-new)
    
    (define/public (custom-write port)   (fprintf port "kodkod+"))    
    (define/public (custom-display port) (custom-write port))
    
    (define/public (shutdown)  (send kodkod-server shutdown))
    
    (define/public (clear)     
      (set! asserts '())
      (set! env #f)
      (when (send kodkod-server initialized?)
        (define stdin (send kodkod-server stdin))
        (cmd [stdin] (kks/clear))
        (flush-output stdin)))
    
    (define/public assert 
      (lambda in (set! asserts (append asserts (filter-asserts in)))))

    (define/public (solve-all) (error 'solve-all "not supported by kodkod-incremental%")) 
    (define/public (debug)     (error 'debug "not supported by kodkod-incremental%")) 
    
    (define/public (solve)    
      (set! asserts (remove-duplicates asserts))
      (cond [(null? asserts) (sat (hash))]
            [(ormap false? asserts) (unsat '(#f))]
            [else 
             (log-time [this] "compilation" : 
                       (cond [env (send kodkod-server write (curryr encode env asserts))]
                             [else (set! env (make-env))
                                   (send kodkod-server write (compile env asserts FALSE 'Glucose))])
                       (set! asserts '()))
             (log-time [this] "solving"     : (send kodkod-server read (curryr decode env)))]))))

(define (compile env asserts cores? solver)
  (define bw (current-bitwidth))
  (when (> bw 32)
    (error 'kodkod "Kodkod supports bitvectors of length up to 32; current bitwidth is set to ~a" bw))
  (curryr encode env asserts  
          ':bitwidth bw
          ':produce-cores cores? 
          ':solver solver
          ':verbosity 0)) 
    
                  
                  
    
    