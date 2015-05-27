#lang s-exp rosette

(require "type.rkt" "errors.rkt"
         rosette/lib/reflect/match (only-in racket/syntax format-symbol)
         (for-syntax (only-in racket/syntax format-id)) 
         (only-in rosette [void rosette-void]))

(provide real-type? real-type-length 
         vector-type? (rename-out [has-vector-type? vector-value?]) vector-type
         vector-select vector-update
         scalar-type? scalar-value? scalar-type
         real-type-of common-real-type void
         bool int float 
         int2 int3 int4 int8 int16 
         float2 float3 float4 float8 float16
         convert_int2 convert_int3 convert_int4 convert_int8 convert_int16 
         convert_float2 convert_float3 convert_float4 convert_float8 convert_float16)

; Each real-type instance represents an OpenCL built-in real type, 
; which can be either a scalar or a vector type. A real type has a 
; name; length (which is 1 for scalar types); a corresponding base 
; (element) type; and a procedure for implicitly converting values 
; to that type, and for constructing values of that type from base 
; components.  The base type of a scalar type is its corresponding 
; Rosette type, and the base type of a vector type is the scalar type 
; of its components. 
;
; Every instance of a real type can be used as a procedure.  Every 
; procedure t takes two forms:  a no-argument form, and a k-argument 
; form, where k is (primitive-type-length t).  The no-argument form 
; returns another procedure, which, when given an argument, implicitly 
; converts that argument to a value of type t, or throws an error if 
; such a conversion is not possible. Valid implicit conversions are 
; described in Ch. 6.2 of opencl-1.2 specification.
;
; When t is a vector type, its k-argument form returns a value of type 
; t if the provided arguments are instances of (type-base t).  When t 
; is a scalar type, the constructor returns a value of type t, only if 
; that value can be constructed from the provided argument without type 
; conversion. 



(struct real-type (base length procedure)
  #:property prop:procedure
  [struct-field-index procedure]
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "~a" (object-name self)))]
  #:methods gen:type
  [(define (type-name self) (object-name self))
   (define (type-base self) (real-type-base self))])



(define-syntax-rule (define-real-type id #:base base #:length length 
                      #:convert  ([pat convert-expr ...] ...)
                      #:construct [(arg ...) construct-expr ...])
  (define id
    (real-type base length
     (let* ([convert (match-lambda [pat convert-expr ...] ...
                                   [v (raise-conversion-error v id)])]
            [id (case-lambda [() convert]
                             [(arg ...) construct-expr ...])])
       id))))

; Returns true if t is a scalar type.
(define (scalar-type? t) (and (real-type? t) (= 1 (real-type-length t))))

; Returns true if t is a vector type.
(define (vector-type? t) (and (real-type? t) (< 1 (real-type-length t))))

; Returns true iff an arithmetic operation on values of types
; t1 and t2 will yield a value of type t2.
(define (real-type<=? t1 t2)
  (and (real-type? t1) (real-type? t2)
       (or (equal? t1 t2) 
           (equal? t1 bool)
           (and (equal? t1 int) (not (equal? t2 bool)))
           (and (equal? t1 float) (equal? (type-base t2) float)))))

; Returns the common real type of the given types, as specified in
; Ch. 6.2.6 of opencl-1.2 specification.  If there is no common
; real type, returns #f.
(define common-real-type
  (case-lambda 
    [(t) (and (real-type? t) t)]
    [(t1 t2) (cond [(real-type<=? t1 t2) t2]
                   [(real-type<=? t2 t1) t1]
                   [else #f])]
    [ts (common-real-type (car ts) (apply common-real-type (cdr ts)))]))



;----------- SCALAR TYPES ----------- 

; We represent OpenCL scalar values as Rosette concrete or symbolic values.
; Since Rosette's symbolic number type does not distinguish between different 
; kinds of number representations (e.g., fixed-point vs. floating point), we 
; rely on compile-time typechecking to enforce this distinction in user code
; where needed. 
;
; When given a symbolic value of type (type-base t) as an argument, a  
; constructor for a scalar type t simply returns that value. 
; When given a concrete value of type t, the constructor checks that the 
; concrete value corresponds to type t and returns it. The constructor will throw an 
; error if given a symbolic or concrete value that cannot be cast to an instance 
; of t without type conversion.

(define-syntax-rule 
  (define-scalar-type id #:base base #:primitive primitive #:convert convert-clauses)
  (define-real-type id 
    #:base base #:length 1 #:convert convert-clauses 
    #:construct [(v) (match v
                       [(? primitive) v]
                       [(term _ (== base)) v]
                       [_ (raise-argument-error 'id (~a 'id) v)])]))

(define-scalar-type bool
  #:base boolean?
  #:primitive boolean?
  #:convert ([(? boolean? v) v]
             [(? number? v)  (! (= v 0))]))

(define-scalar-type int
  #:base number?
  #:primitive fixnum?
  #:convert ([(? boolean? v) (if v 1 0)]
             [(? number? v)  (inexact->exact (truncate v))]))

(define-scalar-type float
  #:base number?
  #:primitive flonum?
  #:convert ([(? boolean? v) (if v 1.0 0.0)]
             [(? number? v)  (exact->inexact v)]))

(define void 
  (let ()
    (struct void ()
      #:property prop:procedure
      (lambda (self)
        (error 'void "cannot cast a value to void"))
      #:methods gen:type
      [(define (type-name self) 'void)
       (define (type-base self) rosette-void)]
      #:methods gen:custom-write
      [(define (write-proc self port mode) 
         (fprintf port "void"))])
    (void)))

;----------- VECTOR TYPES ----------- 

; We represent OpenCL vector values as Racket immutable vector impersonators, 
; with the prop:vector-type property which specifies their OpenCL type.
; The constructor for each vector type t creates an immutable vector value of  
; type t when given (real-type-length t) scalars of type (type-base t).  Otherwise
; it throws an error.

; The property attached to vector values to indicate their 
; OpenCL vector type.
(define-values (prop:vector-type has-vector-type? vector-type)
  (make-impersonator-property 'vector-type))

(define (vector-access v idx val) val)

(define-syntax (define-vector-type stx)
  (syntax-case stx ()
    [(_ id [base length])
     (with-syntax ([(arg ...) (generate-temporaries (make-list (syntax->datum #'length) 'arg))]
                   [(idx ...) (build-list (syntax->datum #'length) values)]
                   [convert_id (format-id #'id "convert_~a" #'id #:source #'id #:props #'id)])
       (syntax/loc stx 
         (begin 
           (define-real-type id
             #:base base #:length length
             #:convert  ([(? boolean? v) (apply id (make-list length (if v ((base) -1) ((base) 0))))]
                         [(? number? v)  (apply id (make-list length ((base) v)))]
                         [(and (? has-vector-type?) (app vector-type (== id)) v) v])
             #:construct [(arg ...) (chaperone-vector (vector-immutable (base arg) ...) 
                                                      vector-access vector-access
                                                      prop:vector-type id)])
           (define (convert_id vec)
             (id ((base) (vector-ref vec idx)) ...)))))]))

(define-vector-type int2  [int 2])
(define-vector-type int3  [int 3])
(define-vector-type int4  [int 4])
(define-vector-type int8  [int 8])
(define-vector-type int16 [int 16])

(define-vector-type float2  [float 2])
(define-vector-type float3  [float 3])
(define-vector-type float4  [float 4])
(define-vector-type float8  [float 8])
(define-vector-type float16 [float 16])

; Returns a scalar or vector value obtained by extracting 
; the values at the given indices from the given vector.
; This procedure requires that 
; (real-type-length type) = (length selector) and 
; (vector-type? type) => (type-base type) = (type-base (vector-type vec)).
(define (vector-select vec selector type)
  (define size (length selector))
  (assert (= (real-type-length type) size))
  (cond [(= size 1) (vector-ref vec (car selector))]
        [else (assert (equal? (type-base type) (type-base (vector-type vec))))
              (apply type (for/list ([idx selector]) (vector-ref vec idx)))]))

; Returns a vector obtained by updating the values at the 
; given indices of the given vector, with the specified value.
; This procedure requires that 
; (type-length (real-type-of val)) = (length selector) and
; (type-base (real-type-of val)) = (type-base (real-type-of vec)).
(define (vector-update vec selector val)
  (define size (length selector))
  (define type (vector-type vec))
  (define len (real-type-length type))
  (define out (vector-copy vec))
  (if (= size 1)
      (vector-set! out (car selector) val)
      (for ([idx selector] [v val])
        (vector-set! out idx v)))
  (apply type (vector->list out)))

;----------- VALUES ----------- 

; Returns the type of the given value or #f if the 
; value does not have an OpenCL real type.
(define (real-type-of v)
  (match v
    [(? boolean?) bool]
    [(term _ (== number?)) int]
    [(? integer?) int]
    [(? real?) float]
    [(? has-vector-type?) (vector-type v)]
    [_ #f]))

; Returns true when given a scalar value, otherwise returns false.
(define (scalar-value? v)
  (scalar-type? (type-of v)))

; Returns the scalar type for the given scalar value v.
(define (scalar-type v)
  (match (real-type-of v)
    [(? scalar-type? t) t]
    [_ (raise-argument-error 'scalar-type "scalar" v)]))

