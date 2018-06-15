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

(define (find-z3 [path #f])
  (cond
    [(and (path-string? path) (file-exists? path)) path]
    [(file-exists? z3-path) z3-path]
    [(file-exists? (path-replace-suffix z3-path ".exe")) (path-replace-suffix z3-path ".exe")]
    [else (or (find-executable-path "z3") #f)]))

(define (make-z3 #:path [path #f])
  (define real-z3-path (find-z3 path))
  (when (and (false? real-z3-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
    (printf "warning: could not find z3 executable at ~a\n" (path->string (simplify-path z3-path))))
  (z3 (server real-z3-path z3-opts set-default-options) '() '() '() (env) '()))
  
(struct z3 (server asserts mins maxs env level)
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<z3>"))]
  #:methods gen:solver
  [
   (define (solver-constructor self)
     make-z3)
   
   (define (solver-features self)
     '(qf_bv qf_uf qf_lia qf_nia qf_lra qf_nra quantifiers optimize unsat-cores))
   
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
     (server-write (z3-server self) (reset))
     (set-default-options (z3-server self)))
   
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
                         (check-sat)))
                 (solver-clear-stacks! self)
                 (read-solution server env)]))
   
   (define (solver-debug self)
     (match-define (z3 server (app unique asserts) _ _ _ _) self)
     (cond [(ormap false? asserts) (unsat (list #f))]
           [else (solver-clear-env! self)
                 (server-write (z3-server self) (reset))
                 (set-core-options (z3-server self))
                 (server-write
                  server
                  (begin (encode-for-proof (z3-env self) asserts)
                         (check-sat)))
                 (read-solution server (z3-env self) #:unsat-core? #t)]))])

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

(define (solver-clear-stacks! self)
  (set-z3-asserts! self '())
  (set-z3-mins! self '())
  (set-z3-maxs! self '()))

(define (solver-clear-env! self)
  (set-z3-env! self (env))
  (set-z3-level! self '()))

; Reads the SMT solution from the server.
; The solution consists of 'sat or 'unsat, followed by  
; followed by a suitably formatted s-expression.  The 
; output of this procedure is a hashtable from constant 
; identifiers to their SMTLib values (if the solution is 'sat);
; a non-empty list of assertion identifiers that form an
; unsatisfiable core (if the solution is 'unsat and a 
; core was extracted); #f (if the solution is 
; 'unsat and no core was extracted); or 'unknown otherwise.
(define (read-solution server env #:unsat-core? [unsat-core? #f])
  (decode
   (parameterize ([current-readtable (make-readtable #f #\# #\a #f)]) ; read BV literals as symbols
     (match (server-read server (read))
       [(== 'sat)
        (server-write server (get-model))
        (let loop ()
          (match (server-read server (read))
            [(list (== 'objectives) _ ...) (loop)]
            [(list (== 'model) def ...)
             (for/hash ([d def] #:when (and (pair? d) (equal? (car d) 'define-fun)))
               (values (cadr d) d))]
            [other (error 'read-solution "expected model, given ~a" other)]))]
       [(== 'unsat)
        (if unsat-core?
            (begin
              (server-write server (get-unsat-core))
              (match (server-read server (read))
                [(list (? symbol? name) ...) name]
                [other (error 'read-solution "expected unsat core, given ~a" other)]))
            'unsat)]
       [(== 'unknown) 'unknown]
       [other (error 'read-solution "unrecognized solver output: ~a" other)]))
   env))
