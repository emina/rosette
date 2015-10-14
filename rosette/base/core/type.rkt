#lang racket

(require racket/generic)
 
(provide 

 define-type       ; (define-type id (#:pred #:default #:merge #:eq? [#:equal?] [#:subtype?]))
 define-primitive-type 
 types             ; (listof type?)

 gen:type
 type?             ; (-> any/c boolean?)
 subtype?          ; (-> type? type? boolean?)
 least-common-supertype ; (-> type? type? type?)
 cast              ; (-> type? typed? any/c)
 type-name         ; (-> type? symbol?)
 type-applicable?  ; (-> type? boolean?)
 type-eq?          ; (-> type? (-> any/c any/c @boolean?)))
 type-equal?       ; (-> type? (-> any/c any/c @boolean?)))
 type-compress     ; (-> type? (listof (cons @boolean? any/c)) (listof (cons @boolean? any/c)))
 type-construct    ; (-> type? (listof any/c) any/c)
 type-deconstruct  ; (-> type? any/c (listof any/c))

 type-of           ; (-> any/c type?)

 gen:typed
 typed?            ; (-> any/c boolean?)
 get-type          ; (-> typed? (or/c #f (-> any/c @boolean?)))
)

#|-----------------------------------------------------------------------------------|#
; The type generic interface defines a symbolic type.  Each value has a type.  Structures that 
; implement the typed? generic interface attach type information directly to their 
; instances.  Types of other values are calculated on the fly.
#|-----------------------------------------------------------------------------------|#

(define-generics typed 
  [get-type typed])

(define-generics type  
  [least-common-supertype type other]
  [cast type v]
  [type-name type]
  [type-applicable? type]
  [type-eq? type u v]
  [type-equal? type u v]
  [type-compress type force? ps]
  [type-construct type vals]
  [type-deconstruct type val])

(define types (list))

(define (set-types! ts)
  ;(printf "~a\n" ts)
  (set! types ts)) 

(struct base-type 
  (name           ; symbol?
   pred           ; contract?
   applicable?    ; boolean?
   eq?            ; (-> any/c any/c boolean?)
   equal?         ; (-> any/c any/c boolean?)
   lcs            ; (-> type? type?)
   cast           ; (-> any/c (values @boolean? any/c))
   compress       ; (->  (listof (cons @boolean? any/c)) (listof (cons @boolean? any/c)))
   construct      ; (-> (listof any/c) any/c)
   deconstruct)   ; (-> any/c (listof any/c))
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
     (fprintf port "~a" (regexp-replace  #px"^@" (symbol->string (base-type-name self)) "")))])

(struct primitive-type base-type ())

(define (no-compress force? ps) ps)

(define (make-type type-constructor name
                   #:pred pred 
                   #:eq? @eq? #:equal? @equal? 
                   #:least-common-supertype lcs
                   #:cast cast 
                   #:applicable? [applicable? #f]
                   #:compress [compress no-compress]
                   #:construct [construct car]
                   #:deconstruct [deconstruct list])
  (let ([t (type-constructor 
            name pred applicable? 
            @eq? @equal? lcs 
            cast compress construct deconstruct)])
    (set-types! (cons t types))
    t))

(define-syntax define-primitive-type 
  (syntax-rules ()
    [(_ id [name] args ...) (define id (make-type primitive-type (quote name) args ...))]
    [(_ id args ...)        (define id (make-type primitive-type (quote id) args ...))]))

(define-syntax define-type
  (syntax-rules ()
    [(_ id [name] args ...) (define id (make-type base-type (quote name) args ...))]
    [(_ id args ...)        (define id (make-type base-type (quote id) args ...))]))

; Returns a type t that accepts the given values, and there is no t' 
; such that t' != t, (subtype? t' t), and t' also accepts the given values. 
; the behavior of this function is undefined if no type accepts all values.
; a type accepts a value v iff (type v) is #t.
(define type-of
  (case-lambda [(v)   (if (typed? v)
                          (get-type v)
                          (for/first ([t types] #:when (t v)) t))]
               [(v u) (least-common-supertype (type-of v) (type-of u))]
               [vs    (for/fold ([t (type-of (car vs))]) ([v (cdr vs)])
                        (least-common-supertype t (type-of v)))]))

(define (subtype? t0 t1)
  (eq? t1 (least-common-supertype t0 t1)))


