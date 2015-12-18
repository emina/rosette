#lang racket

(require racket/syntax 
         (only-in "smtlib2.rkt" Int Real Bool BitVec declare-const define-const assert [< smt/<] [<= smt/<=]) 
         "../../base/core/term.rkt" 
         (only-in "../../base/core/bool.rkt" @boolean?)
         (only-in "../../base/core/num.rkt" @number? current-bitwidth)
         (only-in "../../base/core/bitvector.rkt" bitvector? bitvector-size)
         (only-in "../../base/core/real.rkt" @integer? @real?)
         (only-in "../../base/struct/enum.rkt" enum? enum-size))

(provide (rename-out [make-env env] 
                     [env-decls decls]
                     [env-defs defs])
         ref ref!)

; An environment structure combines a set of 
; declaration and definition dictionaries. The 
; dictionaries map Rosette symbolic constants and 
; expressions, respectively, to SMT identifiers 
; that represent them in the SMT encoding.  Both 
; dictionaries are bijective mappings.
(struct env (decls defs) #:transparent)

; Returns a  fresh empty environment.  
(define (make-env) 
  (env (make-hash) (make-hash)))

; The ref procedure retrieves the SMT identifier for the 
; given Rosette value that is stored in the given environment. 
; If there is no encoding for this value, as given by 
; (dict-has-key? (decls env) val) or (dict-has-key? (defs env) val), 
; this procedure  throws an error.
(define (ref env val)
  (cond [(expression? val) (dict-ref (env-defs env) val)]
        [else (dict-ref (env-decls env) val)]))

(define (smt-id base n) (format-symbol "~a~a" base n))

; Horrible hack to allow testing Int and Real theory before they are properly integrated.
(define (hacked-type-of val)
  (cond [(typed? val) (get-type val)]
        [(boolean? val) @boolean?]
        [(integer? val) (if (infinite? (current-bitwidth)) @integer? @number?)]
        [(real? val) (if (infinite? (current-bitwidth)) @real? @number?)]
        [else (error 'hacked-type-of "value of untranslatable type" val)]))
        
(define (smt-type val)
  (match (hacked-type-of val)
    [(== @boolean?) Bool]
    [(== @number?) (BitVec (current-bitwidth))]
    [(== @integer?) Int]
    [(== @real?) Real]
    [(? bitvector? t) (BitVec (bitvector-size t))]
    [(? enum?) Int]
    [t (error 'smt-type "expected a type that is translatable to SMTLIB, given ~a" t)]))

; The ref! macro retrieves the SMT encoding for 
; the given Rosette value from the given environment. 
; If the environment does not have an encoding for 
; the specified value, it is (optionally) modified to 
; include an encoding for this value.  The macro takes  
; two forms:
; * (ref! env val) returns the identifier that is bound to 
; the Rosette constant val in the environment env.  If no 
; such identifier exists, the macro creates a fresh  
; identifier id; binds val to id in (decls env); declares 
; this binding using declare-const; and returns id.  The 
; identifier takes the form (format-symbol "c~a" i), where 
; i is the size of the (decls env) dictionary in just before 
; the macro is called.
; * (ref! env val enc) returns the SMT encoding that is
; bound to the Rosette value val in the environment env.  
; If env has no encoding for val, and the macro evaluates 
; the provided encoding expression enc.  If the result of 
; evaluating enc is not an s-expression (a pair), that result 
; is returned. Otherwise, the macro uses define-const to 
; introduce a new SMT definition using a fresh identifier id 
; and enc as its body; binds val to id in (defs env); and
; returns id.   The identifier takes the form 
; (format-symbol "e~a" i), where i is the size of the 
; (defs env) dictionary in just before the macro is called. 
(define-syntax ref!
  (syntax-rules ()
    [(_ env val) 
     (let ([decls (env-decls env)]
           [v val])
       (or (dict-ref decls v #f)
           (let ([id (smt-id 'c (dict-count decls))])
             (dict-set! decls v id)
             (declare-const id (smt-type v))
             (assert-invariant id v)
             id)))]
    [(_ env val enc)
     (let ([defs (env-defs env)]
           [v val])
       (or (dict-ref defs v #f) 
           (match enc 
             [(? pair? e) (let ([id (smt-id 'e (dict-count defs))])
                            (dict-set! defs v id)
                            (define-const id (smt-type v) e)
                            id)]
             [e e])))]))

(define (assert-invariant id v)
  (let ([t (type-of v)]) 
    (when (enum? t) 
      (assert (smt/<= 0 id))
      (assert (smt/< id (enum-size t))))))
