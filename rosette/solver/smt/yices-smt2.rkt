#lang racket

(require racket/runtime-path 
         "server.rkt" "env.rkt" 
         "../solver.rkt"
         (prefix-in base/ "base-solver.rkt"))

(provide (rename-out [make-yices-smt2 yices-smt2]) yices-smt2? yices-smt2-available?)

(define-runtime-path yices-smt2-path (build-path ".." ".." ".." "bin" "yices-smt2"))
(define yices-smt2-opts '("--incremental"))

(define (yices-smt2-available?)
  (not (false? (base/find-solver "yices-smt2" yices-smt2-path #f))))
(define default-logic 'QF_BV) ;; YICES_2 needs a default logic set otherwise it will error
(define (make-yices-smt2 [solver #f] #:options [options (hash)] #:logic [logic default-logic] #:path [path #f])
  (define config
    (cond
      [(yices-smt2? solver)
       (base/solver-config solver)]
      [else
       (define real-yices-smt2-path (base/find-solver "yices-smt2" yices-smt2-path path))
       (when (and (false? real-yices-smt2-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
         (error 'yices-smt2 "yices-smt2 binary is not available (expected to be at ~a); try passing the #:path argument to (yices-smt2)" (path->string (simplify-path yices-smt2-path))))
       (base/config options real-yices-smt2-path logic)]))
  (yices-smt2 (server (base/config-path config) yices-smt2-opts (base/make-send-options config)) config '() '() '() (env) '()))

(struct yices-smt2 base/solver ()
  #:property prop:solver-constructor make-yices-smt2
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<yices-smt2>"))]
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
  