#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "env.rkt" 
         "../solver.rkt" "../solution.rkt" 
         (only-in racket [remove-duplicates unique])
         (only-in "smtlib2.rkt" reset set-option check-sat get-model get-unsat-core push pop)
         (only-in "../../base/core/term.rkt" term term? term-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv?)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-z3 z3]) z3?)

(define-runtime-path z3-path (build-path ".." ".." ".." "bin" "z3"))
(define z3-opts '("-smt2" "-in"))

(define (make-z3)
  (unless (file-exists? z3-path)
    (raise-user-error 'make-z3 "Failed to locate z3 binary at '~a'" z3-path))
  (z3 (server z3-path z3-opts) '() '() '() (env) '()))
  
(struct z3 (server asserts mins maxs env level)
  #:mutable
  #:methods gen:solver
  [
   (define (solver-assert self bools)
     (set-z3-asserts! self 
      (append (z3-asserts self)
              (for/list ([b bools] #:unless (equal? b #t))
                (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
                  (error 'assert "expected a boolean value, given ~s" b))
                b))))

   (define (solver-minimize self nums)
     (set-z3-mins! self (append (z3-mins self) (numeric-terms nums 'solver-minimize))))
   
   (define (solver-maximize self nums)
     (set-z3-maxs! self (append (z3-maxs self) (numeric-terms nums 'solver-maximize))))
   
   (define (solver-clear self) 
     (solver-clear-stacks! self)
     (solver-clear-env! self)
     (server-write (z3-server self) (reset-default-options)))
   
   (define (solver-shutdown self)
     (solver-clear self)
     (server-shutdown (z3-server self)))

   (define (solver-push self)
     (match-define (z3 server (app unique asserts) (app unique mins) (app unique maxs) env level) self)
     (server-write
      server
      (begin
        (encode env asserts mins maxs)
        (push)))
     (solver-clear-stacks! self)
     (set-z3-level! self (cons (dict-count env) level)))
   
   (define (solver-pop self [k 1])
     (match-define (z3 server _ _ _ env level) self)
     (when (or (<= k 0) (> k (length level)))
       (error 'solver-pop "expected 1 < k <= ~a, given ~a" (length level) k))
     (server-write server (pop k))
     (solver-clear-stacks! self)
     (for ([lvl level][i k])
       (clear! env lvl))
     (set-z3-level! self (drop level k)))
     
   (define (solver-check self)
     (match-define (z3 server (app unique asserts) (app unique mins) (app unique maxs) env _) self)
     (cond [(ormap false? asserts) (unsat)]
           [else (server-write
                  server
                  (begin (encode env asserts mins maxs)
                         (check-sat)
                         (get-model)))
                 (solver-clear-stacks! self)
                 (server-read server (decode env))]))
   
   (define (solver-debug self)
     (match-define (z3 server (app unique asserts) _ _ _ _) self)
     (cond [(ormap false? asserts) (unsat (list #f))]
           [else (solver-clear-env! self)
                 (server-write (z3-server self) (reset-core-options))
                 (server-write
                  server
                  (begin (encode-for-proof (z3-env self) asserts)
                         (check-sat)
                         (get-unsat-core)))
                 (server-read server (decode (z3-env self)))]))])

(define (reset-default-options)
  (reset)
  (set-option ':produce-unsat-cores 'false)
  (set-option ':auto-config 'true)
  (set-option ':smt.relevancy 2))

(define (reset-core-options)
  (reset)
  (set-option ':produce-unsat-cores 'true)
  (set-option ':auto-config 'false)
  (set-option ':smt.relevancy 0))

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (or (real? t) (bv? t)))
    (match t
      [(term _ (or (== @integer?) (== @real?) (? bitvector?))) t]
      [_ (error caller "expected a numeric term, given ~s" t)])))

(define (solver-clear-stacks! self)
  (set-z3-asserts! self '())
  (set-z3-mins! self '())
  (set-z3-maxs! self '()))

(define (solver-clear-env! self)
  (set-z3-env! self (env))
  (set-z3-level! self '()))
  