#lang racket

(provide 
 op?                          ; (-> any/c boolean?)
 op-name                      ; (-> op? symbol?)
 op-pre                       ; (unconstrained-domain-> @boolean?)
 ops                          ; (listof op?)
 define-op                    ; (define-op id (symbol? (-> (listof any/c) type?) (and/c op? (->* () () #:rest (listof any/c) any)))
 op-arg-type                  ; (-> op? exact-nonnegative-integer? type?)
 op-out-type                  ; (-> op? (listof any/c) type?)
 op/->)                       ; (-> (or/c contract? (listof contract?)) type? (->* ()() #:rest (listof any/c) (values (listof @boolean?) type?)) ))  

(struct op 
  (name   ; symbol?
   arg    ; (-> exact-nonnegative-integer? type?)
   out    ; (unconstrained-domain-> type?)
   pre    ; (unconstrained-domain-> @boolean?)
   proc   ; (unconstrained-domain-> any/c)
   h1 h2)  
  #:property prop:procedure 
  (struct-field-index proc)
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "~a" (op-name self)))]
  #:methods gen:equal+hash
  [(define (equal-proc op1 op2 eq-proc) (eq? op1 op2))
   (define (hash-proc op1 hash-proc) (op-h1 op1))
   (define (hash2-proc op2 hash-proc) (op-h2 op2))])

(define ops (list))

(define (create-op id #:name [name (syntax->datum id)] #:type type #:pre [pre (const #t)] #:op proc) 
  (let ([o  (op name (car type) (cdr type) pre proc 
                (equal-hash-code (symbol->string name))
                (equal-secondary-hash-code (symbol->string name))) ])
    (set! ops (append ops (list o)))
    o))

(define-syntax (define-op stx)
  (syntax-case stx ()
    [(_ id args ...) #`(define id (create-op #'id args ...))]))

(define (op-arg-type op idx)
  ((op-arg op) idx))

(define (op-out-type op args) 
  (apply (op-out op) args))

; Creates an operator type specification using the given argument and result types.
; If arg-types is just one type, then the specification corresponds to an
; arbitrary arity relation over that type.  If the argument is a list of k types, 
; then the specification encodes the given k-ary relation over those k types.
(define-syntax (op/-> stx)
  (syntax-case stx ()
    [(_ (#:rest rest-type) out-type)
     #'(cons (lambda (i) rest-type)
             (lambda xs out-type))]
    [(_ (arg-type ... #:rest rest-type) out-type)
     (with-syntax ([(idx ...) (build-list (length (syntax->list #'(arg-type ...))) values)])
       #'(cons (lambda (i)
                 (case i 
                   [(idx) arg-type] ...
                   [else rest-type]))
               (lambda xs out-type)))]
    [(_ (arg-type ...) out-type) 
     (with-syntax ([(idx ...) (build-list (length (syntax->list #'(arg-type ...))) values)])
       #'(cons (lambda (i)
                 (case i 
                   [(idx) arg-type] ...
                   [else (error 'op-arg-type "Index out of bounds: ~a" i)])) 
               (lambda xs out-type)))]
    [(_ #:arg-type arg-type-proc #:out-type out-type-proc)
     #'(cons arg-type-proc out-type-proc)]))
