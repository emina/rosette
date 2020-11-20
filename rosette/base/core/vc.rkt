#lang racket

(require
  (only-in "term.rkt" term? term-type)
  (only-in "bool.rkt" @boolean? @false? ! && =>)
  "exn.rkt")

(provide @assert @assume vc
         spec-assumes spec-asserts
         spec-tt spec-tt?
         spec-and spec-guard)

(struct spec (assumes asserts) #:transparent)

(define spec-tt (spec #t #t))

(define (spec-tt? s) (equal? s spec-tt))

(define spec-and
  (case-lambda
    [()  spec-tt]
    [(s) s]
    [(s1 s2)  (spec (&& (spec-assumes s1) (spec-assumes s2))
                    (&& (spec-asserts s1) (spec-asserts s2)))]
    [ss (spec (apply && (map spec-assumes ss))
              (apply && (map spec-asserts ss)))]))

(define (spec-guard s g)
  (unless (or (boolean? g) (and (term? g) (equal? (term-type g) @boolean?)))
    (error 'spec-guard "expected a boolean value, given ~a" g))
  (spec (=> g (spec-assumes s)) (=> g (spec-asserts s))))
  

(define vc
  (make-parameter spec-tt
    (lambda (v) (unless (spec? v) (error 'vc "expected a spec, given ~a" v))))) 
                  

(define (assuming s g)  ; g must be a symbolic or concrete boolean
  (spec (&& (spec-assumes s) (=> (spec-asserts s) g)) (spec-asserts s)))

(define (asserting s g) ; g must be a symbolic or concrete boolean 
  (spec (spec-assumes s) (&& (spec-asserts s) (=> (spec-assumes s) g))))

(define (vc! update val raise-vc-err msg)
  (let ([guard (or (eq? val #t) (! (@false? val)))])
    (vc (update (vc) guard))
    (when (false? guard)
      (if (procedure? msg)
          (msg)
          (raise-vc-err (if msg (format "~a" msg) "failed"))))))

(define-syntax (@assert stx)
  (syntax-case stx ()
    [(_ val)     (syntax/loc stx (@assert val #f))]
    [(_ val msg) (syntax/loc stx (vc! asserting val raise-exn:fail:rosette:assertion msg))]))

(define-syntax (@assume stx)
  (syntax-case stx ()
    [(_ val)     (syntax/loc stx (@assume val #f))]
    [(_ val msg) (syntax/loc stx (vc! assuming val raise-exn:fail:rosette:assumption msg))]))
