;;; The portfolio solver is a "meta-solver": it takes other solver instances and combines them into a
;;; single solver, running each query through all the solvers in parallel and returning the first
;;; result.

#lang racket

(require racket/runtime-path
         "server.rkt"
         "env.rkt"
         "../solver.rkt"
         (prefix-in base/ "base-solver.rkt"))

(provide (rename-out [make-portfolio portfolio])
         portfolio?
         portfolio-available?)

; (define-runtime-path cvc5-path (build-path ".." ".." ".." "bin" "cvc5"))
; (define cvc5-opts '("-L" "smt2" "-q" "-m" "-i" "--bv-print-consts-as-indexed-symbols"))

(define (portfolio-available? [solvers '("cvc5")])
  (for/and ([solver solvers])
    (not (false? (base/find-solver solver #f)))))

(define (make-portfolio solvers #:options [options (hash)] #:logic [logic #f] #:path [path #f])
  (define config
    ;;;(cond
    ;;;[(cvc5? solver)
    ;;;(base/solver-config solver)]
    ;;;[else
    ;;;(define real-cvc5-path (base/find-solver "cvc5" cvc5-path path))
    ;;;(when (and (false? real-cvc5-path) (not (getenv "PLT_PKG_BUILD_SERVICE")))
    ;;;(error 'cvc5 "cvc5 binary is not available (expected to be at ~a); try passing the #:path argument to (cvc5)" (path->string (simplify-path cvc5-path))))
    ;;;(base/config options real-cvc5-path logic)]))
    #f) ;;;TODO
  (portfolio (server (base/config-path config)
                     #f ;TODO opts
                     (base/make-send-options config))
             config
             '()
             '()
             '()
             (env)
             '()
             ;;; TODO
             solvers))

(struct portfolio base/solver (solvers)
  #:property prop:solver-constructor
  make-portfolio
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "#<portfolio>"))]
  #:methods gen:solver
  [(define (solver-features self)
     ;;; TODO
     )
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
