#lang racket

(require racket/runtime-path 
         "server.rkt" "env.rkt" 
         "../solver.rkt"
         (prefix-in base/ "base-solver.rkt"))

(provide (rename-out [make-cvc4 cvc4]) cvc4? cvc4-available?)

(define-runtime-path cvc4-path (build-path ".." ".." ".." "bin" "cvc4"))
(define cvc4-opts '("-L" "smt2" "-q" "-m" "-i" "--continued-execution" "--bv-div-zero-const"))

(define (cvc4-available?)
  (not (false? (base/find-solver "cvc4" cvc4-path #f))))

(define (make-cvc4 #:path [path #f])
  (define real-cvc4-path (base/find-solver "cvc4" cvc4-path path))
  (if (and (false? real-cvc4-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
      (error 'cvc4 "cvc4 binary is not available (expected to be at ~a); try passing the #:path argument to (cvc4)" (path->string (simplify-path cvc4-path)))
      (cvc4 (server real-cvc4-path cvc4-opts set-default-options) '() '() '() (env) '())))

(struct cvc4 base/solver ()
  #:property prop:solver-constructor make-cvc4
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "#<cvc4>"))]
  #:methods gen:solver
  [
   (define (solver-features self)
     '(qf_bv qf_uf qf_lia qf_nia qf_lra qf_nra quantifiers))

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
