#lang racket

(require "term.rkt" "reporter.rkt")

(provide union? (rename-out [a-union union])
         union-contents union-type union-guards union-values union-filter
         in-union in-union* in-union-guards in-union-values)

; Represents a symbolic union of guarded values that evaluates either to a 
; single value of a given type, or no value at all.  
; A union evaluates to the value,  if any, whose corresponding guard is true 
; in a given execution.  
; If no such guard exists, the union is an empty (invalid) value.  
; The guards must be constrained  by the semantics of the 
; program so that at most one of them is true in any given execution.  A
; union that is not a λunion cannot contain a procedure as one of its values.
(struct union (contents type) 
  #:transparent
  #:methods gen:typed
  [(define (get-type self) (union-type self))]
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "{")
     (case mode
       [(#t #f) 
        (fprintf port "~a:~a" (equal-hash-code self) (length (union-contents self)))]
       [else
        (let ([vs (union-contents self)])
          (unless (null? vs)
            (parameterize ([error-print-width (max 4 (quotient (error-print-width) (* 2 (length vs))))])
              (fprintf-entry port (car vs) mode)
              (for ([v (cdr vs)])
                (fprintf port " ")
                (fprintf-entry port v mode)))))])
     (fprintf port "}"))])

(define (fprintf-entry port p mode)
  (fprintf port "[")
  (fprintf port "~a" (car p)) 
  (fprintf port " ")
  (fprintf port "~a" (cdr p)) 
  (fprintf port "]")) 

  
(define nil (union '() @any/c))

; A λunion is a symoblic union that must contain a procedure object.  Every 
; λunion is itself an applicable procedure.
(struct λunion union (procedure)
  #:transparent
  #:property prop:procedure [struct-field-index procedure])

(define (make-union . vs)
  ((current-reporter) 'new-union (length vs))
  (match vs
    [(list) nil]
    [_ 
     (let ([vs (sort vs term<? #:key car)]
           [t (apply type-of (map cdr vs))])
       (cond [(type-applicable? t) 
              (λunion vs t (type-compress (lifted-type procedure?) #t vs))]
             [else
              (let ([ps (for/list ([v vs] #:when (procedure? (cdr v))) v)])
                (if (null? ps)
                    (union vs t)
                    (λunion vs t (type-compress (lifted-type procedure?) #t ps))))]))]))
        
(define (union-filter r type)
  (if (or (eq? r nil) (subtype? (union-type r) type))
      r
      (match (for/list ([v (in-union r type)]) v)
        [(list) nil]
        [vs (union vs type)])))

(define (union-guards r) (map car (union-contents r)))
(define (union-values r) (map cdr (union-contents r)))

(define in-union
  (case-lambda [(r) (in-list (union-contents r))]
               [(r type) (sequence-filter (compose type cdr) (in-union r))]))

(define in-union*
  (case-lambda [(r) (in-parallel (in-union-guards r) (in-union-values r))]
               [(r type) (in-parallel (in-union-guards r type) (in-union-values r type))]))

(define in-union-guards
  (case-lambda [(r) (sequence-map car (in-union r))]
               [(r type) (sequence-map car (in-union r type))]))

(define in-union-values
  (case-lambda [(r) (sequence-map cdr (in-union r))]
               [(r type) (sequence-map cdr (in-union r type))]))
 
(define-match-expander a-union
  (syntax-rules (:)
    [(_) (== nil)]
    [(_ vals-pat type-pat) (union vals-pat type-pat)]
    [(_ vals-pat) (union vals-pat _)]
    [(_ : (guard-pat val-pat) ...) 
     (union (list-no-order (cons guard-pat val-pat) ...) _)]
    [(_ : (guard-pat val-pat) ...+ lvp ...) 
     (union (list-no-order (cons guard-pat val-pat) ...+ lvp ...) _)])
  (syntax-id-rules (set!)
    [(a-union v ...) (make-union v ...)]
    [a-union make-union]))
