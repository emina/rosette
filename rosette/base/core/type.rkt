#lang racket

(require racket/generic)
 
(provide 
 gen:typed typed? get-type       
 
 gen:type
 type? type-name         
 type-applicable?  
 type-eq? type-equal?       
 type-compress     
 type-construct type-deconstruct 
 least-common-supertype 
 cast subtype?
 
 type-of @any/c lift-type lifted-type)

#|-----------------------------------------------------------------------------------|#
; The type generic interface defines a symbolic type.  Each value has a type.  Structures that 
; implement the typed? generic interface attach type information directly to their 
; instances.  Types of other values are calculated on the fly.
#|-----------------------------------------------------------------------------------|#

(define-generics typed 
  [get-type typed]) ; (-> typed? type?)

(define-generics type  
  [least-common-supertype type other] ; (-> type? type? type?)
  [cast type v]                       ; (-> type? any/c (values @boolean? any/c))
  [type-name type]                    ; (-> type? symbol?)
  [type-applicable? type]             ; (-> type? boolean?)
  [type-eq? type u v]                 ; (-> type? (-> any/c any/c @boolean?)))
  [type-equal? type u v]              ; (-> type? (-> any/c any/c @boolean?)))
  [type-compress type force? ps]      ; (-> type? (listof (cons @boolean? any/c)) (listof (cons @boolean? any/c)))
  [type-construct type vals]          ; (-> type? (listof any/c) any/c)
  [type-deconstruct type val])        ; (-> type? any/c (listof any/c))

(define (subtype? t0 t1)
  (eq? t1 (least-common-supertype t0 t1)))

#|-----------------------------------------------------------------------------------|#
; Rosette types that lift built-in Racket types are constructed using the lift-type procedure. 
; In the current type system, the following types are expected to be lifted: 
; boolean?, number?, list?, pair?, procedure?, vector?, box?
; See lift-type and type-of for details.
#|-----------------------------------------------------------------------------------|#

(struct base-type 
  (name pred applicable? eq? equal?          
   lcs cast compress construct deconstruct)    
  #:property prop:procedure 
  (struct-field-index pred)
  #:methods gen:type
  [(define (least-common-supertype t0 t1) ((base-type-lcs t0) t1))
   (define (type-name type)               (base-type-name type))
   (define (type-applicable? type)        (base-type-applicable? type))
   (define (type-eq? type u v)            ((base-type-eq? type) u v))   
   (define (type-equal? type u v)         ((base-type-equal? type) u v))   
   (define (cast type v)                  ((base-type-cast type) v))
   (define (type-compress type force? ps) ((base-type-compress type) force? ps))
   (define (type-construct type vals)     ((base-type-construct type) vals))
   (define (type-deconstruct type val)    ((base-type-deconstruct type) val))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (base-type-name self)))])

(define (no-compress force? ps) ps)

; Constructs a new lifted type for the given Racket built-in type, using the 
; following argumets:
;   base                             ; Racket type being lifted.
;   #:is-a? is-a?                    ; Predicate that recognizes concrete and symbolic values of the lifted type.
;   #:eq?  eq?                       ; eq? predicate for the lifted type
;   #:equal? equal?                  ; equal? predicate for the lifted type
;   #:least-common-supertype lcs     ; Procedure for computing the least common supertype of this and the given type.
;   #:cast cas                       ; Procedure for casting a value to this lifted type.
;   #:applicable? [app? #f]          ; Boolean flag indicating if this type represents applicable values.
;   #:compress [compress #f]         ; Procedure for custom [eager] merging of values of this type.
;   #:construct [construct car]      ; Procedure that constructs a value of this type from a list of arguments.
;   #:deconstruct [deconstruct list] ; Procedure that decomposes a value of this type to a list of constituent values.
; A given Racket type cannot be lifted more than once.  That is, multiple attempts to 
; call lift-type with the same base type as argument will result in an error.
; Only these Racket types are expected to be lifted:
; boolean?, number?, list?, pair?, procedure?, vector?, and box?.
(define (lift-type base
                   #:is-a? is-a? 
                   #:eq? is-eq? 
                   #:equal? is-equal? 
                   #:least-common-supertype lcs
                   #:cast cast 
                   #:applicable? [applicable? #f]
                   #:compress [compress #f]
                   #:construct [construct car]
                   #:deconstruct [deconstruct list])
  (unless (hash-has-key? types base)
    (error 'lift "Cannot lift ~a.\nExpected one of ~a." base (hash-keys types)))
  (unless (eq? @any/c (hash-ref types base))
    (error 'lift "Type already lifted: ~a." base))
  (define lifted 
    (base-type (object-name base) is-a? applicable? is-eq? is-equal? 
               lcs cast (or compress no-compress) construct deconstruct))
  (hash-set! types base lifted)
  lifted)

; Universal type that accepts all Racket and Rosette values.  The least-common-supertype 
; method of every type must return #t when given @any? as the argument.
(define @any/c 
  (base-type 'any/c any/c #f eq? equal? 
             (lambda (t) @any/c) (lambda (v) (values #t v)) 
             no-compress car list))

; Binds liftable Racket built-in type predicates to their corresponding Rosette types.
; Initially, all liftable types are bound to @any/c.  See the make-type-of macro.
(define types (make-hash)) 

; Returns the lifted Rosette type corresponding to the given liftable Racket built-in predicate.
(define (lifted-type pred) (hash-ref types pred))

; This macro constructs the type-of procedure.  To allow additional lifted types, 
; add their Racket predicates to the #:base list of the type-of definition.  Note 
; that the order of the #:base types is important---if p is a subtype? of q, then 
; p must be listed before q.
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
   #:base boolean? number? list? pair? procedure? vector? box?))


