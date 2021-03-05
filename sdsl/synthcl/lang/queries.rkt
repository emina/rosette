#lang rosette

(require "util.rkt" racket/stxparam 
         (only-in rackunit test-pred) 
         (for-syntax (only-in racket/syntax with-syntax*))
         (only-in "forms.rkt" range :)
         (only-in rosette/lib/synthax current-grammar-depth print-forms)
         (prefix-in @ (only-in rosette verify synthesize)))

(provide verify synth expected? query-output-port)


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
           (with-terms 
             (parameterize ([current-bitwidth 32]
                            [current-output-port (query-output-port)])
               (printf "Verifying ~a\n" (source-of #'verify))
               (time 
                (or  
                 (for*/or ([id seq] ...)
                   (let ([cex (@verify expr)])
                     (and (sat? cex)                      
                          (print-cex cex (cons 'id id) ...)
                          cex)))
                 (begin 
                   (printf "No counterexample found.\n")
                   (unsat))))))))))]))

; The synthesize form.
(define-syntax (synth stx)
  (syntax-case stx ()
    [(synthesize #:forall [decl ...] #:bitwidth bw #:grammar-depth depth #:ensure expr)
     (with-syntax* ([([id seq] ...) (map id&range (syntax->list #'(decl ...)))]
                    [(tmp ...) (generate-temporaries #'(id ...))])
       (quasisyntax/loc stx 
         (syntax-parameterize
          ([range (syntax-rules () [(_ arg (... ...)) (in-range arg (... ...))])])
          (test-pred
           #,(format "~a" #'synthesize)
           (expected?)
           (with-terms 
             (parameterize ([current-bitwidth bw]
                            [current-grammar-depth depth]
                            [current-output-port (query-output-port)])
               (printf "Synthesizing ~a\n" (source-of #'synthesize))
               (define-values (id ...)
                 (for*/lists (tmp ...) ([id seq] ...) (values id ...)))
               (time
                (let ([m (@synthesize
                          #:forall    (append id ...)
                          #:guarantee (for ([id id] ...) expr))])
                  (if (sat? m) 
                      (print-forms m)
                      (printf "No solution found.\n"))
                  m))))))))]
    [(synthesize #:forall ds #:bitwidth bw #:ensure e) 
     (syntax/loc stx (synthesize #:forall ds #:bitwidth bw #:grammar-depth 3 #:ensure e))]
    [(synthesize #:forall ds #:ensure e) 
     (syntax/loc stx (synthesize #:forall ds #:bitwidth 8 #:grammar-depth 3 #:ensure e))]))


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
  (printf "Counterexample found:\n")
  (for ([name/val cex])
    (printf "\t~a = " (car name/val))
    (write (evaluate (cdr name/val) m))
    (newline)))

