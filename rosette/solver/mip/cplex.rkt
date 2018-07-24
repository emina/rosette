#lang racket

(require racket/generic)
(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "common.rkt"
         "../solver.rkt" "../solution.rkt"
         (only-in rosette symbolics evaluate [= sym/=])
         (only-in racket [remove-duplicates unique])
         (only-in "../../base/core/term.rkt" term term? term-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         (only-in rosette/base/core/term
                  expression expression? constant? term? get-type @app type-of))

(provide (rename-out [make-cplex cplex]) cplex? cplex-available? solver-check-with-init)

(define-runtime-path cplex-path (build-path ".." ".." ".." "bin" "cplex"))
(define cplex-opts '("-f"))

(define (find-cplex [path #f])
  (cond
    [(and (path-string? path) (file-exists? path)) path]
    [(file-exists? cplex-path) cplex-path]
    [else (or (find-executable-path "cplex") #f)]))

(define (cplex-available?)
  (not (false? (find-cplex #f))))

(define (make-cplex [solver #f] #:options [options (hash)] #:path [path #f])
  (define-values (pt verb tim)
    (cond
      [(cplex? solver)
       (values (server-path (cplex-server solver))
               (cplex-verbose solver)
               (cplex-timeout solver))]
      [else
       (define real-cplex-path (find-cplex path))
       (when (and (false? real-cplex-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
         (error 'cplex "cplex binary is not available (expected to be at ~a); try passing the #:path argument to (cplex)" (path->string (simplify-path cplex-path))))
       (values real-cplex-path
               (hash-ref options 'verbose #f)
               (hash-ref options 'timeout #f))]))
  (cplex (server pt cplex-opts) '() '() tim verb))

(define-generics mip-solver
  [solver-check-with-init mip-solver #:mip-start [mip-start] #:mip-sol [mip-sol]])
  
(struct cplex (server asserts objs timeout verbose)
  #:mutable
  #:property prop:solver-constructor make-cplex
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<cplex>"))]
  #:methods gen:mip-solver
  [
   (define (solver-check-with-init self #:mip-start [mip-start #f] #:mip-sol [mip-sol #f])
     (define t0 (current-seconds))
     (match-define (cplex server (app unique asserts) (app unique objs) timeout verbose) self)

     ;; Break multi-objective query into multiple single-objective queries
     ;; because CPLEX doesn't support multi-objective.
     (define (multi-objective asserts objs name2sym temp-sol n)
       ;; Optimize for the first objective on the list.
       (define sol
         (server-run server timeout
                     (encode asserts (car objs))
                     (decode name2sym)
                     (if (> n 0)
                         (build-path temp-sol (format "~a.mst" (sub1 n)))
                         mip-start)
                     (if (and (= (length objs) 1) mip-sol)
                         mip-sol
                         (build-path temp-sol (format "~a.mst" n)))
                     verbose
                     ))
       (cond
         [(empty? (cdr objs)) sol]
         [(unsat? sol) sol]
         [else
          ;; Assert that the current objective must be equal to the found optimal value.
          ;; And exclude the current objective from the objective list.
          (define obj (objective-expr (car objs)))
          (when verbose
            (fprintf (current-error-port)
                     (format "\nAdd constraint ~a\n" (sym/= (evaluate obj sol) obj))))
          (multi-objective (cons (sym/= (evaluate obj sol) obj) asserts)
                           (cdr objs) name2sym temp-sol (add1 n))]))
     
     (cond [(ormap false? asserts)
            (solver-clear-stacks! self)
            (unsat)]
           [else
            (when (= (length objs) 0)
              (raise (exn:fail "MIP solver requires at least one objective." (current-continuation-marks))))
            
            (define name2sym (collect-name2sym (append asserts objs)))
            (define temp (make-temporary-file "cplexsol~a" 'directory))
            (define sol
              (with-handlers ([exn:fail?
                               (λ (e)
                                 (solver-clear-stacks! self)
                                 (delete-directory/files temp)
                                 (raise e))])
                (multi-objective asserts objs name2sym temp 0)))

            (solver-clear-stacks! self)
            (delete-directory/files temp)

            sol
            ]))
   
   ]
  #:methods gen:solver
  [
   (define (solver-features self)
     '(qf_lia qf_lra))
   
   (define (solver-assert self bools)
     (unless (list? bools)
       (raise-argument-error 'solver-assert "(listof boolean?)" bools))
     (set-cplex-asserts! self 
      (append (cplex-asserts self)
              (for/list ([b bools] #:unless (equal? b #t))
                (unless (or (boolean? b) (and (term? b) (equal? @boolean? (term-type b))))
                  (error 'assert "expected a boolean value, given ~s" b))
                b))))

   (define (solver-minimize self nums)
     (set-cplex-objs! self (append (cplex-objs self)
                                   (for/list ([o (numeric-terms nums 'solver-minimize)])
                                     (objective 'min o)))))
   
   (define (solver-maximize self nums)
     (set-cplex-objs! self (append (cplex-objs self)
                                   (for/list ([o (numeric-terms nums 'solver-maximize)])
                                     (objective 'max o)))))
   
   (define (solver-clear self) 
     (solver-clear-stacks! self))
   
   (define (solver-shutdown self)
     (solver-clear self))
     
   (define (solver-check self)
     (solver-check-with-init self))
   
   (define (solver-debug self)
     (error 'solver-debug "debugging isn't supported by solver ~v" self))
   ])

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (real? t))
    (match t
      [(term _ (or (== @integer?) (== @real?))) t]
      [_ (error caller "expected a real or integer term, given ~s" t)])))

(define (solver-clear-stacks! self)
  (set-cplex-asserts! self '())
  (set-cplex-objs! self '()))

(define (collect-name2sym asserts)
  (define name2sym (make-hash))
  (define (collect v)
    (match v
      [(? constant?) (hash-set! name2sym (get-name v) v)]
      [(expression op es ...) (for ([e es]) (collect e))]
      [(objective t e) (collect e)]
      [_ (void)]))
  (for ([a asserts]) (collect a))
  name2sym)
