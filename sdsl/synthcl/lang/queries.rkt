#lang s-exp rosette

(require "util.rkt" racket/stxparam
         (for-syntax (only-in racket/syntax with-syntax*))
         (only-in "forms.rkt" range :)
         (only-in rosette/lib/meta/meta print-forms)
         (only-in rosette/solver/z3/z3 z3%)
         (prefix-in rosette/ (only-in rosette verify synthesize)))

(provide verify synth)

(current-solver (new z3%))

; The verify form.
(define-syntax (verify stx)
  (syntax-case stx ()
    [(verify #:forall [decl ...] #:ensure expr)
     (with-syntax ([([id seq] ...) (map id&range (syntax->list #'(decl ...)))])
       (syntax/loc stx 
         (syntax-parameterize
          ([range (syntax-rules () [(_ arg (... ...)) (in-range arg (... ...))])])
          (configure [bitwidth 32])
          (define clock (current-milliseconds))
          (for* ([id seq] ...)
            (define m (with-handlers ([exn:fail? (lambda (e) #f)]) (rosette/verify expr)))
            (when m 
              (set! clock (- (current-milliseconds) clock))
              (printf "~a counterexample found (~a ms):\n" (source-of #'verify) clock)
              (print-cex m (cons 'id id) ...)
              (error 'verify "counterexample found")))
          (set! clock (- (current-milliseconds) clock))
          (printf "~a no counterexample found (~a ms).\n" (source-of #'verify) clock))))]))

; The synthesize form.
(define-syntax (synth stx)
  (syntax-case stx ()
    [(synthesize #:forall [decl ...] #:bitwidth bw #:ensure expr)
     (with-syntax* ([([id seq] ...) (map id&range (syntax->list #'(decl ...)))]
                    [(tmp ...) (generate-temporaries #'(id ...))])
                   (syntax/loc stx 
                     (syntax-parameterize
                      ([range (syntax-rules () [(_ arg (... ...)) (in-range arg (... ...))])])
                      (configure [bitwidth bw]) ;(current-log-handler (log-handler #:info any/c))
                      (define-values (id ...)
                        (for*/lists (tmp ...) ([id seq] ...) (values id ...)))
                      (define clock (current-milliseconds))
                      (define m (with-handlers ([exn:fail? (lambda (e) #f)]) 
                                  (rosette/synthesize
                                   #:forall    (append id ...)
                                   #:guarantee (for ([id id] ...) expr))))
                      (set! clock (- (current-milliseconds) clock))
                      (cond [m (printf "~a solution found (~a ms):\n" (source-of #'synthesize) clock)
                               (print-forms m)]
                            [else (printf "~a no solution found (~a ms).\n" (source-of #'synthesize) clock)]))))]
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

