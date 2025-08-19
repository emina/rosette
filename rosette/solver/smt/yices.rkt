#lang racket

(require racket/runtime-path 
         "server.rkt" "env.rkt" 
         "../solver.rkt"
         (prefix-in base/ "base-solver.rkt"))

(provide (rename-out [make-yices yices]) yices? yices-available?)

(define-runtime-path bin-path (build-path ".." ".." ".." "bin"))
(define yices-path (build-path bin-path "yices-smt2"))
(define yices-opts '("--incremental"))

(define (yices-available?)
  (not (false? (base/find-solver "yices-smt2" yices-path #f))))
(define default-logic 'QF_BV) ;; Yices2 needs a default logic set otherwise it will error
(define (make-yices [solver #f] #:options [options (hash)] #:logic [logic default-logic] #:path [path #f])
  (define config
    (cond
      [(yices? solver)
       (base/solver-config solver)]
      [else
       (define real-yices-path (base/find-solver "yices-smt2" yices-path path))
       (when (and (false? real-yices-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
         (error 'yices "yices-smt2 binary is not available (expected to be at ~a); try passing the #:path argument to (yices)" (path->string (simplify-path yices-path))))
       (base/config options real-yices-path logic)]))
  (yices (server (base/config-path config) yices-opts (base/make-send-options config)) config '() '() '() (env) '()))

(struct yices base/solver ()
  #:property prop:solver-constructor make-yices
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<yices>"))]
  #:methods gen:solver
  [
   (define (solver-features self)
     '(qf_bv))

   (define (solver-options self)
     (base/solver-options self))

   (define (solver-assert self bools)
     (base/solver-assert self bools))

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
     (base/solver-check self))

   (define (solver-debug self)
     (base/solver-debug self))])

(define (set-default-options server)
  void)
