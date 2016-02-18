#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "env.rkt" 
         "../solver.rkt" "../solution.rkt" 
         "../../base/util/log.rkt" 
         (only-in "smtlib2.rkt" reset)
         (only-in "../../base/core/term.rkt" term? term-type term->datum)
         (only-in "../../base/core/bool.rkt" @boolean?))

(provide (rename-out [make-z3 z3]) z3?)

(define-runtime-path z3-path (build-path ".." ".." ".." "bin" "z3"))
(define z3-opts '("-smt2" "-in"))

(define (make-z3) (z3 (server z3-path z3-opts) '() (env)))
  
(struct z3 (server [asserts #:mutable] [env #:mutable])
  #:methods gen:solver
  [
   (define (solver-add self bools)
     (set-z3-asserts! self 
      (append (z3-asserts self)
              (for/list ([b bools] #:unless (equal? b #t))
                (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
                  (error 'assert "expected a boolean value, given ~s" b))
                b))))
   
   (define (solver-clear self) 
     (set-z3-asserts! self '())
     (set-z3-env! self (env))
     (server-write (z3-server self) (reset)))
   
   (define (solver-shutdown self)
     (solver-clear self)
     (server-shutdown (z3-server self)))
   
   (define (solver-check self)
     (match-define (z3 server (app remove-duplicates asserts) env) self)
     (if (ormap false? asserts) 
         (unsat)
         (parameterize ([current-log-source self])
               (log-time [self] "compilation" : 
                  (server-write server (encode env asserts))
                  (set-z3-asserts! self '()))
               (log-time [self] "solving" : 
                  (server-read server (decode env))))))
   
   (define (solver-localize self) #f)
   ])

