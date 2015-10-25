#lang s-exp rosette

(require "util.rkt" racket/stxparam 
         (only-in rackunit test-pred) 
         (for-syntax (only-in racket/syntax with-syntax*))
         (only-in "forms.rkt" range :)
         (only-in rosette/lib/meta/meta print-forms generate-forms)
         (only-in rosette/solver/smt/z3 z3%)
         (prefix-in rosette/ (only-in rosette verify synthesize)))

(provide verify synth expected? query-output-port)

(define solver (new z3%))

(define expected? (make-parameter any/c))
(define query-output-port (make-parameter (current-output-port)))

; The verify form.
(define-syntax (verify stx)
  (syntax-case stx ()
    [(verify #:forall [decl ...] #:ensure expr)
     (with-syntax ([([id seq] ...) (map id&range (syntax->list #'(decl ...)))])
       (quasisyntax/loc stx 
         (syntax-parameterize
          ([range (syntax-rules () [(_ arg (... ...)) (in-range arg (... ...))])])
          (test-pred
           #,(format "~a" #'verify)
           (expected?)
           (parameterize ([current-bitwidth 32]
                          [current-solver solver]
                          [current-oracle (oracle (current-oracle))]
                          [current-output-port (query-output-port)]
                          [term-cache (hash-copy (term-cache))])
             (printf "Verifying ~a\n" (source-of #'verify))
             (time 
              (or  
               (for*/or ([id seq] ...)
                 (with-handlers ([exn:fail? (lambda (e) #f)])
                   (define cex (rosette/verify expr))
                   (printf "Counterexample found:\n")
                   (print-cex cex (cons 'id id) ...)
                   cex))
               (unsat* "No counterexample found.\n"))))))))]))


; The synthesize form.
(define-syntax (synth stx)
  (syntax-case stx ()
    [(synthesize #:forall [decl ...] #:bitwidth bw #:ensure expr)
     (with-syntax* ([([id seq] ...) (map id&range (syntax->list #'(decl ...)))]
                    [(tmp ...) (generate-temporaries #'(id ...))])
       (quasisyntax/loc stx 
         (syntax-parameterize
          ([range (syntax-rules () [(_ arg (... ...)) (in-range arg (... ...))])])
          (test-pred
           #,(format "~a" #'synthesize)
           (expected?)
           (parameterize ([current-bitwidth bw]
                          [current-solver solver]
                          [current-oracle (oracle (current-oracle))]
                          [current-output-port (query-output-port)]
                          [term-cache (hash-copy (term-cache))])
             (printf "Synthesizing ~a\n" (source-of #'synthesize))
             (define-values (id ...)
               (for*/lists (tmp ...) ([id seq] ...) (values id ...)))
             (time
              (or
               (with-handlers ([exn:fail? (lambda (e) #f)]) 
                 (define m
                   (rosette/synthesize
                    #:forall    (append id ...)
                    #:guarantee (for ([id id] ...) expr)))
                 (print-forms m)
                 m)
               (unsat* "No solution found.\n"))))))))]
    [(synthesize #:forall ds #:ensure e) (syntax/loc stx (synthesize #:forall ds #:bitwidth 8 #:ensure e))]))


; Returns the declared id and the Racket sequence
; of values over which the id ranges.
(define-for-syntax (id&range stx)
  (syntax-case stx (in :)
    [(: type id in rangeExpr) 
     (syntax/loc stx (id rangeExpr))]
    [(: expr ... id)         
     (syntax/loc stx (id (in-value (let () (: expr ... id) id))))]))

; Prints the counterexample by evaluating the given 
; values against the given model.
(define (print-cex m . cex)
  (for ([name/val cex])
    (printf "\t~a = " (car name/val))
    (write (evaluate (cdr name/val) m))
    (newline)))

; Prints the given message and returns (unsat).
(define (unsat* msg)
  (printf msg)
  (unsat))
