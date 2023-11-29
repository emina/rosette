#lang racket

(require racket/runtime-path 
         "server.rkt" "env.rkt" 
         "../solver.rkt"
         (prefix-in base/ "base-solver.rkt"))

(provide (rename-out [make-stp stp]) stp? stp-available?)

(define-runtime-path stp-path (build-path ".." ".." ".." "bin" "stp"))
(define stp-opts '())

(define (stp-available?)
  (not (false? (base/find-solver "stp" stp-path #f))))

(define (make-stp [solver #f] #:options [options (hash)] #:logic [logic #f] #:path [path #f])
  (define config
    (cond
      [(stp? solver)
       (base/solver-config solver)]
      [else
       (define real-stp-path (base/find-solver "stp" stp-path path))
       (when (and (false? real-stp-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
         (error 'stp "stp binary is not available (expected to be at ~a); try passing the #:path argument to (stp)" (path->string (simplify-path stp-path))))
       (base/config options real-stp-path logic)]))
  (stp (server (base/config-path config) stp-opts (base/make-send-options config)) config '() '() '() (env) '()))

(struct stp base/solver ()
  #:property prop:solver-constructor make-stp
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<stp>"))]
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