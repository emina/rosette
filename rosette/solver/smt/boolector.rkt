#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "env.rkt" 
         "../solver.rkt" "../solution.rkt"
         (prefix-in base/ "base-solver.rkt")
         (only-in "smtlib2.rkt" get-model)
         (only-in "../../base/core/term.rkt" term term? term-type constant? expression constant with-terms)
         (only-in "../../base/core/bool.rkt" @boolean? @forall @exists)
         (only-in "../../base/core/bitvector.rkt" bitvector bitvector? bv? bv bv-value @extract @sign-extend @zero-extend @bveq)
         (only-in "../../base/core/function.rkt" function-domain function-range function? function fv)
         (only-in "../../base/core/type.rkt" type-of)
         (only-in "../../base/form/control.rkt" @if))

(provide (rename-out [make-boolector boolector]) boolector? boolector-available?)

(define-runtime-path boolector-path (build-path ".." ".." ".." "bin" "boolector"))
(define boolector-opts '("-m" "--output-format=smt2" "-i"))

(define (boolector-available?)
  (not (false? (base/find-solver "boolector" boolector-path #f))))

(define (make-boolector [solver #f] #:options [options (hash)] #:logic [logic #f] #:path [path #f])
  (define config
    (cond
      [(boolector? solver)
       (base/solver-config solver)]
      [else
       (define real-boolector-path (base/find-solver "boolector" boolector-path path))
       (when (and (false? real-boolector-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
         (error 'boolector "boolector binary is not available (expected to be at ~a); try passing the #:path argument to (boolector)" (path->string (simplify-path boolector-path))))
       (base/config options real-boolector-path logic)]))
  (boolector (server (base/config-path config) boolector-opts (base/make-send-options config)) config '() '() '() (env) '()))

(struct boolector base/solver ()
  #:property prop:solver-constructor make-boolector
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<boolector>"))]
  #:methods gen:solver
  [
   (define (solver-features self)
     '(qf_bv qf_uf))
   
   (define (solver-options self)
     (base/solver-options self))

   (define (solver-assert self bools)
     (base/solver-assert self bools boolector-wfcheck))

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
     (base/solver-check self boolector-read-solution))
   
   (define (solver-debug self)
     (base/solver-debug self))])

(define (set-default-options server)
  void)


(define (valid-type? t)
    (or (equal? t @boolean?)
        (bitvector? t)
        (and (function? t)
             (for/and ([d (in-list (function-domain t))]) (valid-type? d))
             (valid-type? (function-range t)))))
; Check whether a term v is well-formed for Boolector -- it must not mention
; types other than bitvectors, booleans, and uninterpreted functions over those
; types. If not, raise an exception.
(define (boolector-wfcheck v [cache (mutable-set)])
  (unless (set-member? cache v)
    (set-add! cache v)
    (cond
      [(term? v)
       (unless (valid-type? (type-of v))
         (error 'boolector "boolector does not support values of type ~v (value: ~v)" (type-of v) v))
       (match v
         [(expression (or (== @forall) (== @exists)) vars body)
          (error 'boolector "boolector does not support quantified formulas (value: ~v)" v)]
         [(expression (== @extract) i j e)
          (boolector-wfcheck e cache)]
         [(expression (or (== @sign-extend) (== @zero-extend)) v t)
          (boolector-wfcheck v cache)]
         [(expression op es ...)
          (for ([e es]) (boolector-wfcheck e cache))]
         [_ #t])]
      [else
       (unless (or (boolean? v) (bv? v))
         (error 'boolector "boolector does not support value (expected boolean? or bv?): ~v" v))])))


; Reads the SMT solution from the server.
; The solution consists of 'sat or 'unsat, followed by  
; followed by a suitably formatted s-expression.  The 
; output of this procedure is a hashtable from constant 
; identifiers to their SMTLib values (if the solution is 'sat);
; a non-empty list of assertion identifiers that form an
; unsatisfiable core (if the solution is 'unsat and a 
; core was extracted); #f (if the solution is 
; 'unsat and no core was extracted); or 'unknown otherwise.
(define (boolector-read-solution server env)
  (define raw-model
    (parameterize ([current-readtable (make-readtable #f #\# #\a #f)]) ; read BV literals as symbols
       (match (server-read server (read))
         [(== 'sat)
          (server-write server (get-model))
          (match (server-read server (read))
            [(list (== 'model) def ...)
             (for/hash ([d def] #:when (and (pair? d) (equal? (car d) 'define-fun)))
               (values (cadr d) d))]
            [other (error 'read-solution "expected model, given ~a" other)])]
         [(== 'unsat) 'unsat]
         [(== 'unknown) 'unknown]
         [other (error 'read-solution "unrecognized solver output: ~a" other)])))
  ; First, we need to fix up the model's shadowing of incremental variables
  (define stripped-raw-model (if (hash? raw-model) (fixup-incremental-names raw-model) raw-model))
  ; Now decode in an environment with fake types for UFs
  (define m (decode stripped-raw-model (fake-env-types env)))
  ; Finally, fix up the decoded model with the right types
  (fixup-model m))


; Boolector adds a prefix to constant names in incremental mode,
; and repeats bindings for the same constant at different levels.
; For example:
;   (model
;     (define-fun BTOR@5c10 () (_ BitVec 1) #b1)
;     (define-fun BTOR@5c11 () (_ BitVec 1) #b0)
;     (define-fun BTOR@5c13 () (_ BitVec 1) #b1)
;     (define-fun BTOR@6c10 () (_ BitVec 1) #b0)
;     (define-fun BTOR@7c12 () (_ BitVec 1) #b1)
;     (define-fun BTOR@8c13 () (_ BitVec 1) #b0))
; For each constant, we need to choose the binding with the highest such level,
; and then strip the prefix so the names match their actual definitions.
; There are two different styles of prefix, "BTOR@0x" or "BTOR_0@x", depending
; on the version of Boolector (where 0 is the level and x the original name).
(define (strip-BTOR@-prefix-from-define-fun v)
  (match v
    [(list (== 'define-fun) id params ret body)
     `(define-fun ,(regexp-replace #rx"^BTOR((@[0-9]+)|(_[0-9]+@))c" (symbol->string id) "c") ,params ,ret ,body)]
    [_ v]))
(define (BTOR@-level k)
  (let ([match (regexp-match #rx"^BTOR@([0-9]+)(c.*)$" (symbol->string k))])
    (if match
        (values (string->number (second match)) (string->symbol (third match)))
        (let ([match (regexp-match #rx"^BTOR_([0-9]+)@(c.*)$" (symbol->string k))])
          (if match
              (values (string->number (second match)) (string->symbol (third match)))
              (values 0 k))))))
(define (fixup-incremental-names raw-model)
  (define leveled
    (for/fold ([ret (hash)]) ([(k v) raw-model])
      (define-values (l id) (BTOR@-level k))
      (if (> l (car (hash-ref ret id '(-1 -1))))
          (hash-set ret id (cons l (strip-BTOR@-prefix-from-define-fun v)))
          ret)))
  (for/hash ([(k l/v) leveled])
    (values k (cdr l/v))))


; Boolector sometimes interprets booleans as 1-bit bitvectors.
; We need to temporarily make this same transformation in the types
; in an environment before passing it to `decode`, so that decoded
; uninterpreted functions perform the right type casts and checks
; (i.e., they check their inputs are 1-bit BVs instead of booleans).
; Then, once the model has been decoded, we need to undo this transformation,
; so that the final model matches its expected Rosette types.
; More recent versions of Boolector removed this transformation for arity-0
; booleans, but kept it for higher arities (i.e., uninterpreted functions).

; A fake-function? proxies an original function?,
; but with all booleans in the original function's type
; replaced with 1-bit bitvectors.
(struct fake-function function (original) #:transparent)

; Rewrite an env to replace all constant declarations of type function?
; to equivalent fake-function?s that replace booleans with 1-bit bitvectors.
(define (fake-env-types env)
  (with-terms  ; don't pollute the cache with fake constants
    (for/hash ([(decl id) (in-dict env)])
      (values
       (match decl
         [(constant val (function domain range))
          (if (or (member @boolean? domain) (eq? @boolean? range))
              (constant (gensym)
                        (fake-function (for/list ([a (in-list domain)])
                                         (if (eq? @boolean? a) (bitvector 1) a))
                                       (if (eq? @boolean? range) (bitvector 1) range)
                                       decl))
              decl)]
         [_ decl])
       id))))

; Replace all values with type fake-function? in a model with their
; original types, wrapping their procedures in a cast from booleans to
; 1-bit bitvectors where necessary.
(define bv1 (bitvector 1))
(define (fixup-model m)
  (match m
    [(model dict)
     (sat (for/hash ([(var val) (in-dict dict)])
            (match (type-of var)
              [(== @boolean?)
               (values var (if (bv? val)
                               (not (= (bv-value val) 0))
                               val))]
              [(fake-function fake-domain fake-range original)
               (match-define (function orig-domain orig-range) (type-of original))
               (match val
                 [(fv type)
                  (define inner
                    (lambda args
                      (apply val
                             (for/list ([a (in-list args)]
                                        [ot (in-list orig-domain)]
                                        [ft (in-list fake-domain)])
                               (if (and (eq? ot @boolean?) (eq? ft bv1))
                                   (@if a (bv 1 1) (bv 0 1))
                                   a)))))
                  (define outer
                    (if (and (eq? orig-range @boolean?) (eq? fake-range bv1))
                        (lambda args (@bveq (apply inner args) (bv 1 1)))
                        inner))
                  (values original (fv (type-of original) outer))])]
              [_ (values var val)])))]
    [_ m]))
