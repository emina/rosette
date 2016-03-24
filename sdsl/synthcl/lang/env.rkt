#lang rosette

(require syntax/id-table "types.rkt" "builtins.rkt")

(provide current-env env lookup bind)

; Current environment consists of a list of env objects.  Lexical 
; scoping is implemented in the usual way, with the first environment 
; in the list treated as the top frame.  The builtin constant and 
; function names are always bound.
(define current-env
  (make-parameter 
   (list (make-free-id-table builtins #:phase 10))	
   (lambda (e)
     (unless (and (mutable-free-id-table? e) (zero? (dict-count e)))
       (raise-argument-error 'current-env "an empty mutable-free-id-table?" e))
     (cons e (current-env)))))
        
; Returns a new typechecking environment, which is a mutable-free-id-table? 
; from variable identifiers to type annotations.  
(define (env) (make-free-id-table #:phase 10))

; Retrieves the typed binding for id, if any, from the current environment. 
(define (lookup id)
  (type-set 
   id
   (or 
    (for/or ([e (current-env)])
      (dict-ref e id #f))
    (raise-syntax-error #f "unbound identifier" id))))
    
; Binds the identifier id to the given type in the top frame of the current environment. 
(define (bind id t [stx id])
  (define e (car (current-env)))
  (when (dict-has-key? e id)
    (raise-syntax-error #f "duplicate declaration" stx id))
  (dict-set! e id t))



