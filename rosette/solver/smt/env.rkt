#lang racket

(require racket/syntax 
         (only-in "smtlib2.rkt" Int Real Bool BitVec
                  declare-const declare-fun define-const assert
                  [< smt/<] [<= smt/<=]) 
         "../../base/core/term.rkt" 
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/bitvector.rkt" bitvector? bitvector-size)
         (only-in "../../base/core/real.rkt" @integer? @real?))

(provide (rename-out [make-hash env]) ref!)

(define (smt-id base n) (format-symbol "~a~a" base n))

(define (smt-type t)
  (match t
    [(== @boolean?) Bool]
    [(== @integer?) Int]
    [(== @real?) Real]
    [(? bitvector? t) (BitVec (bitvector-size t))]
    [_ (error 'smt-type "expected primitive-solvable? type, given ~a" t)]))

; The ref! macro retrieves the SMT encoding for 
; the given Rosette value from the given environment. 
; If the environment does not have an encoding for 
; the specified value, it is (optionally) modified to 
; include an encoding for this value.  The macro takes  
; two forms:
; * (ref! env val) returns the identifier that is bound to 
; the Rosette constant val in the environment env.  If no 
; such identifier exists, the macro creates a fresh  
; identifier id; binds val to id in env; declares 
; this binding using declare-const; and returns id.  The 
; identifier takes the form (format-symbol "c~a" i), where 
; i is the size of the env dictionary just before 
; the macro is called.
; * (ref! env val enc) returns the SMT encoding that is
; bound to the Rosette value val in the environment env.  
; If env has no encoding for val, and the macro evaluates 
; the provided encoding expression enc.  If the result of 
; evaluating enc is not an s-expression (a pair), that result 
; is returned. Otherwise, the macro uses define-const to 
; introduce a new SMT definition using a fresh identifier id 
; and enc as its body; binds val to id in env; and
; returns id.   The identifier takes the form 
; (format-symbol "e~a" i), where i is the size of the 
; env dictionary in just before the macro is called. 
(define-syntax ref!
  (syntax-rules ()
    [(_ env val) 
     (let ([defs env]
           [v val])
       (or (dict-ref defs v #f)         
           (let ([id (smt-id 'c (dict-count defs))]
                 [t (term-type v)])
             (dict-set! defs v id)
             (declare-fun id (map smt-type (solvable-domain t)) (smt-type (solvable-range t)))
             id)))]
    [(_ env val enc)
     (let ([defs env]
           [v val])
       (or (dict-ref defs v #f) 
           (match enc 
             [(? pair? e) (let ([id (smt-id 'e (dict-count defs))])
                            (dict-set! defs v id)
                            (define-const id (smt-type (type-of v)) e)
                            id)]
             [e e])))]))
