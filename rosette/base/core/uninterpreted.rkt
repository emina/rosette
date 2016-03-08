#lang racket

(require "term.rkt" "equality.rkt" "merge.rkt" "bool.rkt")

(provide (rename-out [uf uninterpreted] [lut LUT])
         uninterpreted? LUT? LUT-sig LUT-map LUT-default)

#|-----------------------------------------------------------------------------------|#
; An uninterpreted function (UF) is a function whose interpretation is not fixed.
; Instead, it is determined by the solver. The domain of an uninterpreted function (UF)
; is a list of type?, and the range of a UF is a type?.  Two
; UFs are equal? iff they have the same identifier, domain, and range.
;
; The interpretation of a UF is a function that implements a finite lookup table (LUT).
; Each LUT has the same signature (identifier, domain, and range) as the uninterpreted
; function for which it is providing an interpretation.  Each LUT also has a safe and
; an unsafe procedure associated with it.  Two LUTs are equal? iff they are eq?.
#|-----------------------------------------------------------------------------------|#

(struct uninterpreted (identifier domain range safe unsafe)
  #:property prop:procedure 
  (struct-field-index safe)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (id->string (uninterpreted-identifier self))))]
  #:methods gen:function
  [(define (function-identifier self) (uninterpreted-identifier self))
   (define (function-domain self) (uninterpreted-domain self))
   (define (function-range self) (uninterpreted-range self))
   (define (function-unsafe self) (uninterpreted-unsafe self))]
  #:methods gen:equal+hash
  [(define (equal-proc u1 u2 rec=?)
     (and (rec=? (function-identifier u1) (function-identifier u2))
          (rec=? (function-range u1) (function-range u2))
          (rec=? (function-domain u1) (function-domain u2))))
   (define (hash-proc u1 rec-hash)
     (rec-hash (list (function-identifier u1)
                     (function-domain u1)
                     (function-range u1))))
   (define (hash2-proc u1 rec-hash)
     (rec-hash (list (function-identifier u1)
                     (function-domain u1)
                     (function-range u1))))])

(define (make-uninterpreted id dom ran)
  (define k (length dom))
  (define name (string->symbol (id->string id)))
  (define f
    (uninterpreted
     id dom ran 
     (procedure-rename
      (procedure-reduce-arity
       (lambda args
         (apply expression f (for/list ([a args][t dom]) (type-cast t a name))))
       k)
      name)
     (procedure-reduce-arity
      (lambda args
        (apply expression f args))
      k)))
  f)

(define-match-expander uf
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat ...) #'(uninterpreted pat ... _ _)]))
  (syntax-id-rules ()
    [(_ id dom ran) (make-uninterpreted id dom ran)]
    [_ make-uninterpreted]))

(struct LUT (sig map default safe unsafe)
  #:property prop:procedure 
  (struct-field-index safe)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (LUT-sig self)))]
  #:methods gen:function
  [(define (function-identifier self) (function-identifier (LUT-sig self)))
   (define (function-domain self) (function-domain (LUT-sig self)))
   (define (function-range self) (function-range (LUT-sig self)))
   (define (function-unsafe self) (LUT-unsafe self))])

; Creates a lookup table interpretation for the given uninterpreted function,
; using the provided list of input/output pairs and the default output value.
(define (make-LUT sig tbl default)
  (define (unsafe . args)
    (define parts (for/list ([kv tbl]) (cons (@equal? (car kv) args) (cdr kv))))
    (apply merge* (cons (! (apply || (map car parts))) default) parts))
  (define (safe . args)
    (apply unsafe (for/list ([a args][t (function-domain sig)])
                    (type-cast t a))))
  (define k (length (function-domain sig)))
  (LUT sig tbl default
       (procedure-rename (procedure-reduce-arity safe k) (object-name sig))
       (procedure-reduce-arity unsafe k)))

(define-match-expander lut
  (lambda (stx)
    (syntax-case stx ()
      [(_ pat ...) #'(LUT pat ... _ _)]))
  (syntax-id-rules ()
    [(_ sig tbl default) (make-LUT sig tbl default)]
    [_ make-LUT]))    
