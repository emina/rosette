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

(provide (rename-out [make-cvc4 cvc4]) cvc4? cvc4-available?)

(define-runtime-path cvc4-path (build-path ".." ".." ".." "bin" "cvc4"))
(define cvc4-opts '("-L" "smt2" "-q" "-m" "-i" "--continued-execution" "--bv-div-zero-const"))

(define (find-cvc4 [path #f])
  (cond
    [(and (path-string? path) (file-exists? path)) path]
    [(file-exists? cvc4-path) cvc4-path]
    [(find-executable-path "cvc4") => identity]
    [else #f]))

(define (cvc4-available?)
  (not (false? (find-cvc4 #f))))

(define (make-cvc4 #:path [path #f])
  (define real-cvc4-path (find-cvc4 path))
  (if (and (false? real-cvc4-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
      (error 'cvc4 "cvc4 binary is not available (expected to be at ~a); try passing the #:path argument to (cvc4)" (path->string (simplify-path cvc4-path)))
      (cvc4 (server real-cvc4-path cvc4-opts set-default-options) '() '() '() (env) '())))

(struct cvc4 (server asserts mins maxs env level)
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<cvc4>"))]
  #:methods gen:solver
  [
   (define (solver-constructor self)
     make-cvc4)
   
   (define (solver-features self)
     '(qf_bv qf_uf qf_lia qf_nia qf_lra qf_nra quantifiers))

   (define (solver-assert self bools)
     (set-cvc4-asserts! self 
      (append (cvc4-asserts self)
              (for/list ([b bools] #:unless (equal? b #t))
                (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
                  (error 'assert "expected a boolean value, given ~s" b))
                b))))

   (define (solver-minimize self nums)
     (unless (null? nums)
       (error 'solver-minimize "cvc4 optimization isn't supported")))
   
   (define (solver-maximize self nums)
     (unless (null? nums)
       (error 'solver-maximize "cvc4 optimization isn't supported")))
   
   (define (solver-clear self)
     (solver-shutdown self))
   
   (define (solver-shutdown self)
     (solver-clear-stacks! self)
     (solver-clear-env! self)
     (server-shutdown (cvc4-server self)))

   (define (solver-push self)
     (match-define (cvc4 server (app unique asserts) (app unique mins) (app unique maxs) env level) self)
     (server-write
      server
      (begin
        (encode env asserts mins maxs)
        (push)))
     (solver-clear-stacks! self)
     (set-cvc4-level! self (cons (dict-count env) level)))
   
   (define (solver-pop self [k 1])
     (match-define (cvc4 server _ _ _ env level) self)
     (when (or (<= k 0) (> k (length level)))
       (error 'solver-pop "expected 1 < k <= ~a, given ~a" (length level) k))
     (server-write server (pop k))
     (solver-clear-stacks! self)
     (for ([lvl level][i k])
       (clear! env lvl))
     (set-cvc4-level! self (drop level k)))
     
   (define (solver-check self)
     (match-define (cvc4 server (app unique asserts) (app unique mins) (app unique maxs) env _) self)
     (cond [(ormap false? asserts) (unsat)]
           [else (server-write
                  server
                  (begin (encode env asserts mins maxs)
                         (check-sat)))
                 (solver-clear-stacks! self)
                 (read-solution server env)]))
   
   (define (solver-debug self)
     (error 'solver-debug "cvc4 debug not supported"))])

(define (set-default-options server)
  void)

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (or (real? t) (bv? t)))
    (match t
      [(term _ (or (== @integer?) (== @real?) (? bitvector?))) t]
      [_ (error caller "expected a numeric term, given ~s" t)])))

(define (solver-clear-stacks! self)
  (set-cvc4-asserts! self '())
  (set-cvc4-mins! self '())
  (set-cvc4-maxs! self '()))

(define (solver-clear-env! self)
  (set-cvc4-env! self (env))
  (set-cvc4-level! self '()))

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
