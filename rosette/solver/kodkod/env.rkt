#lang racket

(require (only-in "kks.rkt"  declare-const define-const assert r e f i tupleset tuple NONE) 
         "univ.rkt" "../../base/term.rkt" "../../base/bool.rkt" "../../base/num.rkt")

(provide (rename-out [make-env env] [env-univ univ] [env-defs defs] [env-decls decls]) 
         env? ref! ref) 

; An environment structure combines universe, a 
; dictionary of declarations, and a dictionary of 
; definitions.
(struct env (univ decls defs f i e) #:transparent)

; Returns a fresh environment.
(define (make-env)
  (env (universe) 
       (make-hash) 
       (make-hash) 
       (id-generator f) 
       (id-generator i) 
       (id-generator e)))

(define (id-generator proc) 
  (let ([idx 0])
    (procedure-rename
     (lambda () 
       (begin0 (proc idx) 
               (set! idx (add1 idx))))
     (object-name proc))))

; The ref procedure retrieves the Kodkod encoding for the 
; given Rosette value that is stored in the given environment. 
; If there is no encoding for this value, as given by 
; (dict-has-key? (decls env) val) or (dict-has-key? (defs env) val), 
; this procedure  throws an error.
(define (ref env val)
  (cond [(expression? val) (dict-ref (env-defs env) val)]
        [else (dict-ref (env-decls env) val)]))

; The ref! macro retrieves the Kodkod encoding for 
; the given Rosette value from the given environment. 
; If the environment does not have an encoding for 
; the specified value, it is (optionally) modified to 
; include an encoding for this value.  The macro takes  
; three forms:
; * (ref! env val #:bound bound ...) returns the relation 
; that is bound to the Rosette constant val in the environment 
; env.  If no such relation exists, the macro creates a fresh relation 
; identifier id; binds val to id in (decls env); declares
; this binding using (declare-const id bound ...); and returns id. 
; * (ref! env val #:bound bound ... #:invariant expr) returns the relation 
; that is bound to the Rosette constant val in the 
; environment env.  If no such relation exists, the macro 
; creates a fresh relation identifier id; binds val to 
; id in (decls env); declares this binding using (declare-const id bound ...);  
; asserts the result of evaluating expr; and returns id.  
; * (ref! env val enc) returns the Kodkod encoding that is
; bound to the Rosette value val in the environment env.  
; If env has no encoding for val, and the macro evaluates 
; the provided encoding expression enc.  If the result of 
; evaluating enc is not an s-expression (a pair), that result 
; is returned. Otherwise, the macro uses define-const to 
; introduce a new Kodkod definition using a fresh identifier id 
; and enc as its body; binds val to id in (defs env); and
; returns id. 
(define-syntax ref!
  (syntax-rules ()
    [(_ env val #:bound bound ... #:invariant expr) 
     (let ([decls (env-decls env)]
           [v val])
       (or (dict-ref decls v #f)
           (let ([id (r (dict-count decls))])
             (dict-set! decls v id)
             (declare-const id bound ...) 
             (assert-invariant expr)
             id)))]
    [(_ env val #:bound bound ...) (ref! env val #:bound bound ... #:invariant #t)]
    [(_ env val enc)
     (let ([defs (env-defs env)]
           [v val])
       (or (dict-ref defs v #f)
           (match enc
             [(? pair? e) (let ([id (gen-id env (type-of v))])
                            (dict-set! defs v id)
                            (define-const id e)
                            id)]
             [e e])))]))
  
(define (gen-id env type)
  (match type
    [(== @boolean?) ((env-f env))]
    [(== @number?)  ((env-i env))]
    [_                 ((env-e env))]))

(define (assert-invariant inv)
  (unless (eq? #t inv) 
    (assert inv)))
