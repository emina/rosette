#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt"
         "common.rkt" "smt-simplify.rkt" "mip-converter.rkt"
         "../solver.rkt" "../solution.rkt"
         (only-in rosette symbolics evaluate [= sym/=])
         (only-in racket [remove-duplicates unique])
         (only-in "../../base/core/term.rkt" term term? term-type)
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bv?)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-cplex cplex]) cplex?)

(define-runtime-path cplex-path (build-path ".." ".." ".." "bin" "cplex"))
(define cplex-opts '("-f"))

(define (env) (make-hash))

(define (make-cplex)
  (define real-cplex-path
    ;; Check for 'cplex', else print a warning
    (if (file-exists? cplex-path)
      cplex-path
      (begin
            (printf "warning: could not find z3 executable in '~a'"
                    (path->string (simplify-path (path->directory-path cplex-path))))
            cplex-path)))
  (cplex (server real-cplex-path cplex-opts) '() '() (env) '()))

  
(struct cplex (server asserts objs env level)
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<cplex>"))]
  #:methods gen:solver
  [
   (define (solver-assert self bools)
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
     (solver-clear-stacks! self)
     (solver-clear-env! self))
   
   (define (solver-shutdown self)
     (solver-clear self))

   (define (solver-push self)
     (raise (exn:fail "cplex: solver-push: unimplemented")))
   
   (define (solver-pop self [k 1])
     (raise (exn:fail "cplex: solver-pop: unimplemented")))
     
   (define (solver-check self)
     (match-define (cplex server (app unique asserts) (app unique objs) env _) self)
     
     (define (multi-objective asserts objs convert)
       (define sol
         (server-run server
                     (encode env asserts (car objs))
                     (decode env convert)))
       (cond
         [(empty? (cdr objs)) sol]
         [else
          (define obj (objective-expr (car objs)))
          (fprintf (current-error-port) (format "\nAdd constraint ~a\n" (sym/= (evaluate obj sol) obj)))
          (multi-objective (cons (sym/= (evaluate obj sol) obj) asserts)
                           (cdr objs) convert)]))
     
     (cond [(ormap false? asserts) (unsat)]
           [else
            (when (= (length objs) 0)
              (raise (exn:fail "MIP solver requires at least one objective." (current-continuation-marks))))
            
            ;; step 1: simply equation (flatten)
            (define sim-asserts (simplify asserts))
            (define sim-objs
              (for/list ([o objs])
                (objective (objective-type o) (simplify-expression (objective-expr o)))))

            ;; step 2: convert SMT to MIP
            (define convert (smt->mip sim-asserts sim-objs))
            (define mip-asserts (converter-asserts convert))
            (define mip-objs (converter-objs convert))

            (fprintf (current-error-port) (format "SMT: asserts=~a vars=~a\n" (length sim-asserts) (length (symbolics sim-asserts))))
            (fprintf (current-error-port) (format "MIP: asserts=~a vars=~a\n" (length mip-asserts) (length (symbolics mip-asserts))))

            ;; step 3: solve
            (define sol (multi-objective mip-asserts mip-objs convert))
            (solver-clear-stacks! self)
            sol
            ]))
   
   (define (solver-debug self)
     (raise (exn:fail "cplex: solver-debug: unimplemented" (current-continuation-marks))))

   
   ])

(define (numeric-terms ts caller)
  (for/list ([t ts] #:unless (real? t))
    (match t
      [(term _ (or (== @integer?) (== @real?))) t]
      [_ (error caller "expected a real or integer term, given ~s" t)])))

(define (solver-clear-stacks! self)
  (set-cplex-asserts! self '())
  (set-cplex-objs! self '()))

(define (solver-clear-env! self)
  (set-cplex-env! self (env))
  (set-cplex-level! self '()))
  
