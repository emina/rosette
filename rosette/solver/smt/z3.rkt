#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "env.rkt" 
         "../solver.rkt" "../solution.rkt" 
         (only-in racket [remove-duplicates unique])
         (only-in "smtlib2.rkt" reset set-option)
         (only-in "../../base/core/term.rkt" term term? term-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv?)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-z3 z3]) z3?)

(define-runtime-path z3-path (build-path ".." ".." ".." "bin" "z3" "bin" "z3"))
(define z3-opts '("-smt2" "-in"))

(define (make-z3) (z3 (server z3-path z3-opts) '() '() '() (env)))
  
(struct z3 (server [asserts #:mutable] [mins #:mutable] [maxs #:mutable] [env #:mutable])
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
     (set-z3-asserts! self '())
     (set-z3-mins! self '())
     (set-z3-maxs! self '())
     (set-z3-env! self (env))
     (server-write (z3-server self) (reset-default-options)))
   
   (define (solver-shutdown self)
     (solver-clear self)
     (server-shutdown (z3-server self)))
   
   (define (solver-check self)
     (match-define (z3 server (app unique asserts) (app unique mins) (app unique maxs) env) self)
     (cond [(ormap false? asserts) (unsat)]
           [else (server-write server (encode env asserts mins maxs))
                 (set-z3-asserts! self '())
                 (set-z3-mins! self '())
                 (set-z3-maxs! self '())
                 (server-read server (decode env))]))
   
   (define (solver-debug self)
     (match-define (z3 server (app unique asserts) _ _ _) self)
     (cond [(ormap false? asserts) (unsat (list #f))]
           [else (set-z3-env! self (env))
                 (server-write (z3-server self) (reset-core-options))
                 (server-write server (encode-for-proof (z3-env self) asserts))
                 (server-read server (decode (z3-env self)))]))
   ])

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
