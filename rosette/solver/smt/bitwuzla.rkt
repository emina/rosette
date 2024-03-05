;;; Requires Bitwuzla 0.1.0 or later.
#lang racket

(require racket/runtime-path 
         "server.rkt" "cmd.rkt" "env.rkt" 
         "../solver.rkt" "../solution.rkt"
         (prefix-in base/ "base-solver.rkt")
         (only-in "../../base/core/term.rkt" term term? term-type constant? expression constant with-terms)
         (only-in "../../base/core/bool.rkt" @boolean? @forall @exists)
         (only-in "../../base/core/bitvector.rkt" bitvector bitvector? bv? bv bv-value @extract @sign-extend @zero-extend @bveq)
         (only-in "../../base/core/function.rkt" function-domain function-range function? function fv)
         (only-in "../../base/core/type.rkt" type-of)
         (only-in "../../base/form/control.rkt" @if))

(provide (rename-out [make-bitwuzla bitwuzla]) bitwuzla? bitwuzla-available?)

(define-runtime-path bin-path (build-path ".." ".." ".." "bin"))
(define bitwuzla-path (build-path bin-path "bitwuzla"))
(define bitwuzla-opts '("-m"))

(define (bitwuzla-available?)
  (not (false? (base/find-solver "bitwuzla" bitwuzla-path #f))))

(define (make-bitwuzla [solver #f] #:options [options (hash)] #:logic [logic #f] #:path [path #f])
  (define config
    (cond
      [(bitwuzla? solver)
       (base/solver-config solver)]
      [else
       (define real-bitwuzla-path (base/find-solver "bitwuzla" bitwuzla-path path))
       (when (and (false? real-bitwuzla-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
         (error 'bitwuzla "bitwuzla binary is not available (expected to be at ~a); try passing the #:path argument to (bitwuzla)" (path->string (simplify-path bitwuzla-path))))
       (base/config options real-bitwuzla-path logic)]))
  (bitwuzla (server (base/config-path config) bitwuzla-opts (base/make-send-options config)) config '() '() '() (env) '()))

(struct bitwuzla base/solver ()
  #:property prop:solver-constructor make-bitwuzla
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<bitwuzla>"))]
  #:methods gen:solver
  [
   (define (solver-features self)
     '(qf_bv))
   
   (define (solver-options self)
     (base/solver-options self))

   (define (solver-assert self bools)
     (base/solver-assert self bools bitwuzla-wfcheck))

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
   
   (define (solver-check self [read-solution base/read-solution])
     (base/solver-check self read-solution))
   
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
; Check whether a term v is well-formed for bitwuzla -- it must not mention
; types other than bitvectors, booleans, and uninterpreted functions over those
; types. If not, raise an exception.
(define (bitwuzla-wfcheck v [cache (mutable-set)])
  (unless (set-member? cache v)
    (set-add! cache v)
    (cond
      [(term? v)
       (unless (valid-type? (type-of v))
         (error 'bitwuzla "bitwuzla does not support values of type ~v (value: ~v)" (type-of v) v))
       (match v
         [(expression (or (== @forall) (== @exists)) vars body)
          (error 'bitwuzla "bitwuzla does not support quantified formulas (value: ~v)" v)]
         [(expression (== @extract) i j e)
          (bitwuzla-wfcheck e cache)]
         [(expression (or (== @sign-extend) (== @zero-extend)) v t)
          (bitwuzla-wfcheck v cache)]
         [(expression op es ...)
          (for ([e es]) (bitwuzla-wfcheck e cache))]
         [_ #t])]
      [else
       (unless (or (boolean? v) (bv? v))
         (error 'bitwuzla "bitwuzla does not support value (expected boolean? or bv?): ~v" v))])))
