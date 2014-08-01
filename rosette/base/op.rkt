#lang racket

(provide 
 op?                          ; (-> any/c boolean?)
 op-id                        ; (-> op? identifier?)
 op-name                      ; (-> op? symbol?)
 op-pre                       ; (unconstrained-domain-> @boolean?)
 ops                          ; (listof op?)
 define-op                    ; (define-op id (symbol? (-> (listof any/c) type?) (and/c op? (->* () () #:rest (listof any/c) any)))
 op/arg-types                 ; (->* (op?) () #:rest (listof any/c) (listof @type?))
 op/type                      ; (->* (op?) () #:rest (listof any/c) type?)
 op/->)                       ; (-> (or/c contract? (listof contract?)) type? (->* ()() #:rest (listof any/c) (values (listof @boolean?) type?)) ))  

(struct op 
  (id     ; identifier?
   name   ; symbol?
   type   ; (cons (unconstrained-domain-> (listof type?)) (unconstrained-domain-> type?))
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
  (let ([o  (op id name type pre proc ;(procedure-rename proc name)
                (equal-hash-code (symbol->string name))
                (equal-secondary-hash-code (symbol->string name))) ])
    (set! ops (append ops (list o)))
    o))

(define-syntax (define-op stx)
  (syntax-case stx ()
    [(_ id args ...) #`(define id (create-op #'id args ...))]))

(define (op/arg-types op . args) 
  (apply (car (op-type op)) args))

(define (op/type op . args) 
  (apply (cdr (op-type op)) args))

; Creates an operator type specification using the given argument and result types.
; If arg-types is just one type, then the specification corresponds to an
; arbitrary arity relation over that type.  If the argument is a list of k types, 
; then the specification encodes the given k-ary relation over those k types.
(define-syntax (op/-> stx)
  (syntax-case stx ()
    [(_ (#:rest rest-type) out-type)
     #'(cons (let ([type1 (list rest-type)]
                   [type2 (list rest-type rest-type)]
                   [type-proc (const rest-type)]) 
               (case-lambda [() '()]
                            [(param) type1]
                            [(param0 param1) type2]
                            [params (map type-proc params)])) 
             (const out-type))]
    [(_ (arg-type ... #:rest rest-type) out-type)
     (with-syntax ([(param ...) (generate-temporaries #'(arg-type ...))])
       #'(cons (lambda (param ... . rest) (list* arg-type ... (map (const rest-type) rest))) 
               (const out-type)))]
    [(_ (arg-type ...) out-type) 
     (with-syntax ([(param ...) (generate-temporaries #'(arg-type ...))])
       #'(cons (let ([types (list arg-type ...)]) 
                 (lambda (param ...) types)) 
               (const out-type)))]
    [(_ args pre out-type)
     #'(cons (lambda args pre) (lambda args out-type))]))
