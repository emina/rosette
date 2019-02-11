#lang racket

(require racket/runtime-path 
         "server.rkt" "env.rkt" "cmd.rkt"
         "../solver.rkt"
         (only-in "smtlib2.rkt" get-model echo)
         (prefix-in base/ "base-solver.rkt")
         (only-in "../../base/core/term.rkt" term? expression)
         (only-in "../../base/core/bool.rkt" @forall @exists)
         (only-in "../../base/core/bitvector.rkt"  @integer->bitvector))

(provide (rename-out [make-yices yices]) yices? yices-available?)

(define-runtime-path yices-path (build-path ".." ".." ".." "bin" "yices-smt2"))
(define yices-opts '("--incremental"))

(define (yices-available?)
  (not (false? (base/find-solver "yices" yices-path #f))))

(define (make-yices [solver #f] #:options [options (hash)] #:logic [logic 'ALL] #:path [path #f])
  (define config
    (cond
      [(yices? solver)
       (base/solver-config solver)]
      [else
       (define real-yices-path (base/find-solver "yices" yices-path path))
       (when (and (false? real-yices-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
         (error 'yices "yices binary is not available (expected to be at ~a); try passing the #:path argument to (yices)" (path->string (simplify-path yices-path))))
       (base/config options real-yices-path logic)]))
  (yices (server (base/config-path config) yices-opts (base/make-send-options config)) config '() '() '() (env) '()))

(struct yices base/solver ()
  #:property prop:solver-constructor make-yices
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<yices>"))]
  #:methods gen:solver
  [
   (define (solver-features self)
     '(qf_bv qf_uf qf_lia qf_lra))
   
   (define (solver-options self)
     (base/solver-options self))

   (define (solver-assert self bools)
     (base/solver-assert self bools yices-wfcheck))

   (define (solver-minimize self nums)
     (base/solver-minimize self nums))
   
   (define (solver-maximize self nums)
     (base/solver-maximize self nums))
   
   (define (solver-clear self)
     (base/solver-clear self))
   
   (define (solver-shutdown self)
     (base/solver-shutdown self))

   (define (solver-push self)
     (base/solver-push self))
   
   (define (solver-pop self [k 1])
     (base/solver-pop self k))
   
   (define (solver-check self)
     (base/solver-check self yices-read-solution))
   
   (define (solver-debug self)
     (base/solver-debug self))])


; Check whether a term v is well-formed for Yices -- it must not 
; use integer->bitvector, which Yices doesn't support natively,
; nor a quantifier.
(define (yices-wfcheck v [cache (mutable-set)])
  (unless (set-member? cache v)
    (set-add! cache v)
    (when (term? v)
      (match v
        [(expression (or (== @forall) (== @exists)) vars body)
         (error 'yices "yices does not support quantified formulas (value: ~v)" v)]
        [(expression (== @integer->bitvector) rest ...)
         (error 'yices "yices does not support integer->bitvector (value: ~v)" v)]
        [(expression op es ...)
         (for ([e es]) (yices-wfcheck e cache))]
        [_ #t]))))


; Reads the SMT solution from the server.
; The solution consists of 'sat or 'unsat, followed by  
; followed by a suitably formatted s-expression.  The 
; output of this procedure is a hashtable from constant 
; identifiers to their SMTLib values (if the solution is 'sat);
; a non-empty list of assertion identifiers that form an
; unsatisfiable core (if the solution is 'unsat and a 
; core was extracted); #f (if the solution is 
; 'unsat and no core was extracted); or 'unknown otherwise.
;
; For Yices, we need to adapt to its strange way of printing models,
; which are just a list of terms of the form (= x y), with no delimiters
; for the start and end of a model. We print a "done" symbol to detect when
; to stop reading terms. We also need to handle Yices' format for printing
; uninterpreted functions:
; (= x @fun_6)
; (function @fun_6
;  (type (-> int int))
;  (= (@fun_6 0) 0)
;  (= (@fun_6 1) 1)
;  (default 2))
; We convert these models into define-fun forms, with each case becoming
; a nested ITE. The define-fun forms have fake type information, since
; decode doesn't use it (preferring to use the type info from env).
(define (yices-read-solution server env)
  (define raw-model
    (parameterize ([current-readtable (make-readtable #f #\# #\a #f)]) ; read BV literals as symbols
       (match (server-read server (read))
         [(== 'sat)
          (server-write server
            (get-model)
            (echo "done"))
          (let loop ([model (hash)][functions (hash)])
            (match (server-read server (read))
              [(list (== '=) var val)
               (loop (hash-set model var `(define-fun ,var () X ,val)) functions)]
              [(list (== 'function) var type cases ...)
               (define args (for/list ([x (in-range (- (length (cadr type)) 2))]) (gensym)))
               (define (call->guard call)
                 (if (equal? (length args) 1)
                     `(= ,(car args) ,(cadr call))
                     `(and ,@(for/list ([a args][c (cdr call)]) `(= ,a ,c)))))
               (define fun
                 (let inner ([cases cases])
                   (match (car cases)
                     [(list (== 'default) val) val]
                     [(list (== '=) call val)
                      `(ite ,(call->guard call) ,val ,(inner (cdr cases)))])))
               (loop model
                     (hash-set functions var `(define-fun ,var ,(for/list ([a args]) `(,a X)) X ,fun)))]
              ['done
               (for/hash ([(var defn) (in-dict model)])
                 (define val (fifth defn))
                 (values var (hash-ref functions val defn)))]
              [other (error 'read-solution "expected model, given ~a" other)]))]
         [(== 'unsat) 'unsat]
         [(== 'unknown) 'unknown]
         [other (error 'read-solution "unrecognized solver output: ~a" other)])))
  (decode raw-model env))
