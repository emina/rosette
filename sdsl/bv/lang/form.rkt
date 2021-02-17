#lang racket

(require "program.rkt" "core.rkt" "fragment.rkt" "log.rkt"
         rosette/query/eval
         rosette/solver/solution
         (only-in rosette/base/core/term with-terms)
         (for-syntax racket/syntax))

(provide define-fragment synthesize-fragment bvlib verbose?)

(define-syntax (define-fragment stx)
  (syntax-case stx ()
    [(_ (id param ...) #:implements spec #:library lib-expr)
     #'(define-fragment (id param ...) #:implements spec #:library lib-expr #:minbv 4)]
    [(_ (id param ...) #:implements spec #:library lib-expr #:minbv minbv)
     #`(define-values (id #,(format-id #'id "~a-stx" #'id #:source #'id)) 
         (synthesize-fragment (id param ...) 
                              #:implements spec 
                              #:library lib-expr 
                              #:minbv minbv))]))

; Returns two values:  a procedure named id and a syntactic representation of that procedure.
; The procedure's body is a loop-free code fragment, built from the provided library of components,
; that implements the given spec.
(define-syntax (synthesize-fragment stx)
  (syntax-case stx ()
    [(_ (id param ...) #:implements spec #:library lib-expr)
     #'(synthesize-fragment (id param ...) #:implements spec #:library lib-expr #:minbv 4)]
    [(_ (id param ...) #:implements spec #:library lib-expr #:minbv minbv)
     #`(with-terms
         (begin
           (bv-info "synthesizing ~a" #'id)
           (define impl (prog* #,(length (syntax->list #'(param ...))) lib-expr))
           (define-values (val cpu real gc) (time-apply ∃∀-solve (list impl spec minbv)))     
           (match (car val)
             [(? sat? sol) 
              (bv-info "synthesized ~a (ms): cpu = ~a, real = ~a, gc = ~a" #'id cpu real gc)
              (define impl-stx (fragment->syntax (list (quote-syntax param) ...) impl sol))
              (values (procedure-rename (eval impl-stx) 'id) impl-stx)]
             [_ 
              (error 'synthesize-fragment 
                     "unable to synthesize ~a (ms): cpu = ~a, real = ~a, gc = ~a"
                     #'id cpu real gc)])))]))

(define-syntax-rule (bvlib [{id ...} n] ...)
  (flatten
   (append (for/list ([i (list #'id ...)][p (list id ...)]) 
             (make-list n (op i p))) ...)))



