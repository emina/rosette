#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "env.rkt" 
         "../solver.rkt" "../solution.rkt"
         (prefix-in base/ "base-solver.rkt")
         (only-in racket [remove-duplicates unique])
         (only-in "smtlib2.rkt" reset set-option check-sat)
         (only-in "../../base/core/term.rkt" term term? term-type)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv?)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-z3 z3]) z3?)

(define-runtime-path z3-path (build-path ".." ".." ".." "bin" "z3"))
(define z3-opts '("-smt2" "-in"))

(define (make-z3 #:path [path #f])
  (define real-z3-path (base/find-solver "z3" z3-path path))
  (when (and (false? real-z3-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
    (printf "warning: could not find z3 executable at ~a\n" (path->string (simplify-path z3-path))))
  (z3 (server real-z3-path z3-opts set-default-options) '() '() '() (env) '()))
  
(struct z3 base/solver ()
  #:mutable
  #:property prop:solver-constructor make-z3
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<z3>"))]
  #:methods gen:solver
  [
   (define (solver-features self)
     '(qf_bv qf_uf qf_lia qf_nia qf_lra qf_nra quantifiers optimize unsat-cores))

   (define (solver-assert self bools)
     (base/solver-assert self bools))

   (define (solver-minimize self nums)
     (base/set-solver-mins! self (append (base/solver-mins self) (numeric-terms nums 'solver-minimize))))
   
   (define (solver-maximize self nums)
     (base/set-solver-maxs! self (append (base/solver-maxs self) (numeric-terms nums 'solver-maximize))))
   
   (define (solver-clear self) 
     (base/solver-clear-stacks! self)
     (base/solver-clear-env! self)
     (server-write (base/solver-server self) (reset))
     (set-default-options (base/solver-server self)))
   
   (define (solver-shutdown self)
     (solver-clear self)
     (server-shutdown (base/solver-server self)))

   (define (solver-push self)
     (base/solver-push self))
   
   (define (solver-pop self [k 1])
     (base/solver-pop self k))
   
   (define (solver-check self)
     (base/solver-check self))

   (define (solver-debug self)
     (match-define (z3 server (app unique asserts) _ _ _ _) self)
     (cond [(ormap false? asserts) (unsat (list #f))]
           [else (base/solver-clear-env! self)
                 (server-write (base/solver-server self) (reset))
                 (set-core-options (base/solver-server self))
                 (server-write
                  server
                  (begin (encode-for-proof (base/solver-env self) asserts)
                         (check-sat)))
                 (base/read-solution server (base/solver-env self) #:unsat-core? #t)]))])

(define (set-default-options server)
  (server-write server
    (set-option ':produce-unsat-cores 'false)
    (set-option ':auto-config 'true)
    (set-option ':smt.relevancy 2)
    (set-option ':smt.mbqi.max_iterations 10000000)))

(define (set-core-options server)
  (server-write server
    (set-option ':produce-unsat-cores 'true)
    (set-option ':auto-config 'false)
    (set-option ':smt.relevancy 0)))

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (or (real? t) (bv? t)))
    (match t
      [(term _ (or (== @integer?) (== @real?) (? bitvector?))) t]
      [_ (error caller "expected a numeric term, given ~s" t)])))
