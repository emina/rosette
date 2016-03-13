#lang racket

(require racket/generic)
 
(provide 
 gen:typed typed? get-type       
 
 gen:type
 type? type-name type-cast       
 type-applicable?  
 type-eq? type-equal?       
 type-compress     
 type-construct type-deconstruct 
 least-common-supertype 
 subtype?

 gen:solvable
 solvable? solvable-default solvable-domain solvable-range
 primitive-solvable?
 
 type-of @any/c lifted-type define-lifted-type)

#|-----------------------------------------------------------------------------------|#
; The type generic interface defines a symbolic type.  Each value has a type.
; Structures that implement the typed? generic interface attach type information
; directly to their instances.  Types of other values are calculated on the fly.
;
; The solvable generic interface acts as a marker for types that are supported by
; the underlying constraint solver.  The solvable-default method of a solvable type T
; returns a default value of type T that may be used for binding constants that are
; otherwise unconstrained.  The solvable-domain method returns a list of solvable?
; types that are not applicable; that is, (type-applicable? T) returns #f.  The
; solvable-range method returns a single solvable? non-applicable type.  If the type
; T is not applicable, then solvable-range returns T itself and solvable-domain returns
; the empty list.
#|-----------------------------------------------------------------------------------|#

(define-generics typed 
  [get-type typed]) ; (-> typed? type?)

(define-generics type  
  [least-common-supertype type other] ; (-> type? type? type?)
  [type-cast type val [caller]]       ; (-> type? any/c symbol? any/c)
  [type-name type]                    ; (-> type? symbol?)
  [type-applicable? type]             ; (-> type? boolean?)
  [type-eq? type u v]                 ; (-> type? (-> any/c any/c @boolean?)))
  [type-equal? type u v]              ; (-> type? (-> any/c any/c @boolean?)))
  [type-compress type force? ps]      ; (-> type? (listof (cons @boolean? any/c)) (listof (cons @boolean? any/c)))
  [type-construct type vals]          ; (-> type? (listof any/c) any/c)
  [type-deconstruct type val])        ; (-> type? any/c (listof any/c))

(define (subtype? t0 t1)
  (eq? t1 (least-common-supertype t0 t1)))

(define-generics solvable
  [solvable-default solvable] ; (-> (and/c solvable? type?) any/c)
  [solvable-domain solvable]  ; (-> (and/c solvable? type?) (listof primitive-solvable?))
  [solvable-range solvable])  ; (-> (and/c solvable? type?) primitive-solvable?)

(define (primitive-solvable? t)
  (and (solvable? t) (type? t) (not (type-applicable? t))))

; Defines a new lifted type for the given Racket built-in type, using the 
; following arguments:
;   id                               ; Identifier for the lifted type.
;   #:base base                      ; Racket type being lifted.
;   #:is-a? is-a?                    ; Predicate that recognizes concrete and symbolic values of the lifted type.
;   #:methods ([method-id expr] ...) ; Definitions of gen:type methods, including at least cast. This can
;                                    ; optionally include gen:solvable methods.
; A given Racket type cannot be lifted more than once.  That is, multiple attempts to 
; call define-lifted-type with the same base type as argument will result in an error.
; Only these Racket types are expected to be lifted:
; boolean?, integer?, real?, list?, pair?, procedure?, vector?, and box?.
(define-syntax (define-lifted-type stx)
  (syntax-case stx ()
    [(_ id #:base base #:is-a? is-a? #:methods defs)       
       #`(begin
           (unless (hash-has-key? types base)
             (error 'lift "Cannot lift ~a.\nExpected one of ~a." base (hash-keys types)))
           (unless (eq? @any/c (hash-ref types base))
             (error 'lift "Type already lifted: ~a." base))
           (define id (make-lifted-type #:base base #:is-a? is-a? #:methods defs))       
           (hash-set! types base id))]))

(define-syntax (make-lifted-type stx)
  (syntax-case stx ()
    [(_ #:base base #:is-a? is-a? #:methods defs)
     (let* ([methods (for/hash ([expr (syntax->list #'defs)])
                      (with-syntax ([(define (method arg ...) body ...) expr])
                        (values (syntax->datum #'method) #'(lambda (arg ...) body ...))))]
            [required (lambda (m) (or (hash-ref methods m #f)
                                      (raise-syntax-error 
                                       'define-lifted-type 
                                       (format "missing required method definition ~a" m))))])
                              
       #`(let ()
           (struct lifted (pred)
             #:property prop:procedure [struct-field-index pred]
             #:methods gen:custom-write
             [(define (write-proc self port mode) (fprintf port "~a" 'base))]
             #,@(if (hash-has-key? methods 'solvable-default)
                    #`(#:methods gen:solvable
                        [(define solvable-default #,(hash-ref methods 'solvable-default))
                         (define solvable-domain  #,(hash-ref methods 'solvable-domain #'(lambda (self) null)))
                         (define solvable-range   #,(hash-ref methods 'solvable-range #'(lambda (self) self)))])
                    #`())
             #:methods gen:type
             [(define least-common-supertype #,(hash-ref methods 'least-common-supertype 
                                                         #'(lambda (self other) (if (equal? self other) self @any/c))))
              (define type-cast              #,(required 'type-cast))
              (define type-name              #,(hash-ref methods 'type-name #'(lambda (self) 'base)))
              (define type-applicable?       #,(hash-ref methods 'type-applicable? #'(lambda (self) #f)))             
              (define type-eq?               #,(hash-ref methods 'type-eq? #'(lambda (self u v) (eq? u v))))                  
              (define type-equal?            #,(hash-ref methods 'type-equal? #'(lambda (self u v) (equal? u v))))               
              (define type-compress          #,(hash-ref methods 'type-compress #'(lambda (self force? ps) ps)))   
              (define type-construct         #,(hash-ref methods 'type-construct #'(lambda (self vals) (car vals))))      
              (define type-deconstruct       #,(hash-ref methods 'type-deconstruct #'(lambda (self val) (list val))))])      
           (lifted is-a?)))]))

; Universal type that accepts all Racket and Rosette values.  The least-common-supertype 
; method of every type must return #t when given @any? as the argument.
(define @any/c 
  (make-lifted-type
   #:base any/c 
   #:is-a? (const #t)
   #:methods
   [(define (least-common-supertype self other) self)
    (define (type-cast self v [caller 'type-cast]) v)]))

; Binds liftable Racket built-in type predicates to their corresponding Rosette types.
; Initially, all liftable types are bound to @any/c.  See the make-type-of macro.
(define types (make-hash)) 

; Returns the lifted Rosette type corresponding to the given liftable Racket built-in predicate.
(define (lifted-type pred) (hash-ref types pred))

; This is a hacked type-of implementation to allow testing Int and Real theories 
; before they are properly integrated.  The current-bitwidth parameter controls 
; whether we are using the old int/real semantics (default) or not.
(define-syntax-rule (typechecker #:base id ...)
  (begin
    (hash-set! types id @any/c) ...
    (case-lambda
      [(v) (cond [(typed? v) (get-type v)]
                 [(id v) (hash-ref types id)] ...
                 [else @any/c])]
      [(v u) (least-common-supertype (type-of v) (type-of u))]
      [vs    (for/fold ([t (type-of (car vs))]) ([v (cdr vs)] #:break (eq? t @any/c))
               (least-common-supertype t (type-of v)))])))

; The type-of procedure a type t that accepts the given values, and there is no t' 
; such that t' != t, (subtype? t' t), and t' also accepts the given values.  
(define type-of 
  (typechecker 
   #:base boolean? integer? real? list? pair? procedure? vector? box?))

