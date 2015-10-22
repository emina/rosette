#lang racket

(require "location.rkt" "instruction.rkt" "solver.rkt" "fragment.rkt"
         rosette/base/core/num
         rosette/base/form/define
         rosette/query/eval
         rosette/query/state rosette/base/form/state
         rosette/solver/solution
         rosette/base/util/log
         (for-syntax racket/syntax))

(provide define-fragment synthesize-fragment)

(define verbose? (make-parameter #t))

(current-log-handler (log-handler #:info (curry equal? 'define-fragment)))

(define-syntax (define-fragment stx)
  (syntax-case stx ()
    [(_ (id param ...) #:ensures post #:library lib-expr)
     #'(define-fragment (id param ...) #:requires (const #t) #:ensures post #:library lib-expr)]
    [(_ (id param ...) #:requires pre #:ensures post #:library lib-expr)
     #'(define-fragment (id param ...) #:min-bitwidth 4 #:requires pre #:ensures post #:library lib-expr)]
    [(_ (id param ...) #:min-bitwidth k #:ensures post #:library lib-expr)
     #'(define-fragment (id param ...) #:min-bitwidth k #:requires (const #t) #:ensures post #:library lib-expr)]
    [(_ (id param ...) #:min-bitwidth k #:requires pre #:ensures post #:library lib-expr )
     #`(define-values (id #,(format-id #'id "~a-stx" #'id #:source #'id)) 
         (synthesize-fragment (id param ...) #:min-bitwidth k #:requires pre #:ensures post #:library lib-expr))]))

; Returns two values:  a procedure named id and a syntactic representation of that procedure.
; The procedure's body is a loop-free code fragment, built from the provided library of components,
; that satisfies the give post condition on all inputs that satisfy the given pre-condition.
(define-syntax (synthesize-fragment stx)
  (syntax-case stx ()
    [(_ (id param ...) #:ensures post #:library lib-expr)
     #'(synthesize-fragment (id param ...) #:requires (const #t) #:ensures post #:library lib-expr)]
    [(_ (id param ...) #:requires pre #:ensures post #:library lib-expr)
     #'(synthesize-fragment (id param ...) #:min-bitwidth 4 #:requires pre #:ensures post #:library lib-expr)]
    [(_ (id param ...) #:min-bitwidth k #:ensures post #:library lib-expr)
     #'(synthesize-fragment (id param ...) #:min-bitwidth k #:requires (const #t) #:ensures post #:library lib-expr)]
    [(_ (id param ...) #:min-bitwidth k #:requires pre #:ensures post #:library lib-expr )
     #`(parameterize ([current-oracle (oracle)]
                      [current-solution (empty-solution)]
                      [current-log-source (if (verbose?) 'define-fragment #f)]
                      [inputs #,(length (syntax->list #'(param ...)))])
         (log-info "synthesizing ~a" #'id)
         (define-symbolic* param @number?) ...
         (define lib (optimize-library #:inputs (list param ...) #:library lib-expr))
         (define-values (val cpu real gc) 
           (time-apply ex-all-solve (list (list param ...) pre post lib k)))
         (define fun-stx (fragment->syntax #:solution (current-solution) 
                                           #:inputs (list (quote-syntax param) ...)
                                           #:library lib))
         (log-info "synthesized ~a (ms): cpu = ~a, real = ~a, gc = ~a" #'id cpu real gc)
         (values (procedure-rename (eval fun-stx) 'id) fun-stx))]))

(define (optimize-library #:inputs inputs #:library lib)
  (let ([consts (for/list ([inst lib] #:when (null? (instruction-inputs inst))) inst)])
    (if (null? consts)
        lib
        (append (for/list ([inst consts] [i (in-naturals (length inputs))])
                  (new-instruction (instruction-id inst)
                                   (instruction-proc inst)
                                   (instruction-inputs inst)
                                   (location i)))
                (remove* consts lib)))))
