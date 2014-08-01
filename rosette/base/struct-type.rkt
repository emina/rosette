#lang racket


(require (for-syntax "lift.rkt" racket/syntax) 
         (only-in "effects.rkt" apply!)
         "term.rkt"  "lift.rkt" "safe.rkt"
         (only-in "bool.rkt" || && and-&&)
         (only-in "any.rkt" @any?)
         (only-in "procedure.rkt" @procedure?)
         (only-in "merge.rkt" merge merge*)
         (only-in "union.rkt" union union? in-union-guards)
         (only-in "equality.rkt" @equal? @eq?)
         (only-in "generic.rkt" do-cast))

(provide @struct-predicate @make-struct-field-accessor @make-struct-field-mutator)

(define (@make-struct-field-mutator lifted? i field-id)
  (let ([native? (struct-type-native? lifted?)]
        [setter (make-struct-field-mutator (struct-type-set! lifted?) i field-id)]
        [getter (make-struct-field-accessor (struct-type-ref lifted?) i field-id)])
    (procedure-rename 
     (lambda (receiver value) 
       (if (native? receiver)
           (apply! setter getter receiver value) 
           (match (coerce receiver lifted? (object-name setter))
             [(? native? r) (apply! setter getter receiver value)]
             [(union rs) (for ([r rs]) 
                           (apply! setter getter (cdr r) (merge (car r) value (getter (cdr r)))))])))
     (object-name setter))))

(define (@make-struct-field-accessor lifted? i field-id)
  ;(printf "@make-struct-field-accessor ~a ~a ~a\n" lifted? i field-id)
  (let ([native? (struct-type-native? lifted?)]
        [getter (make-struct-field-accessor (struct-type-ref lifted?) i field-id)])
    (procedure-rename 
     (lambda (receiver) 
       (if (native? receiver)
           (getter receiver)
           (match (coerce receiver lifted? (object-name getter))
             [(? native? r) (getter r)]
             [(union r) (merge** r getter)])))
     (object-name getter))))

(define (@struct-predicate struct:super is-a? make ref set! field-count immutable? transparent? procedure? equal+hash)
  ;(printf "@struct-type:\n")
  ;(printf "  super=~a, ?=~a, make=~a\n  ref=~a, set!=~a, field-count=~a\n" struct:super is-a? make ref set! field-count)
  ;(printf "  immutable?=~a, transparent?=~a, procedure?=~a\n  equal+hash=~a\n" immutable? transparent? procedure? equal+hash) 
  (define (t? v)
    (match v
      [(? is-a?) #t] 
      [(and (? typed? v) (app get-type t)) 
       (or (and t (subtype? t st)) 
           (and (union? v) (apply || (for/list ([g (in-union-guards v st)]) g))))]
      [_ #f]))
  (define super (and struct:super (typed? struct:super) (get-type struct:super)))
  (define st
    (struct-type 
     (procedure-rename t? (object-name is-a?))
     super is-a? make ref set! field-count 
     (and immutable? (implies super (struct-type-immutable? super))) 
     (and transparent? (implies super (struct-type-transparent? super))) 
     (or procedure? (and super (struct-type-procedure? super)))
     equal+hash))
  st)

(struct struct-type (pred super native? make ref set! fields immutable? transparent? procedure? equal+hash)
  #:property prop:procedure
  [struct-field-index pred]
  #:methods gen:type
  [(define (least-common-supertype type other)
     (or (and (eq? type other) type)
         (and (eq? other @procedure?) (struct-type-procedure? type) @procedure?)
         (and (not (struct-type? other)) @any?)
         (least-common-super-struct-type type other)
         (and (struct-type-procedure? type) (struct-type-procedure? other) @procedure?)
         @any?))
   (define (type-name type)               
     (object-name (struct-type-native? type)))
   (define (type-applicable? type)
     (struct-type-procedure? type))
   (define (type-eq? type u v)            
     (or (eq? u v) 
         (and (struct-type-immutable? type) 
              (struct-type-transparent? type) 
              (not (struct-type-equal+hash type)) 
              (struct=? type u v @eq?))))
   (define (type-equal? type u v)   
     (struct=? type u v @equal?))  
   (define (cast type v)                  
     (do-cast v (struct-type-native? type) type))
   (define (type-compress type force? ps) 
     (if (or force? (and (struct-type-immutable? type) 
                         (struct-type-transparent? type) 
                         (not (struct-type-equal+hash type))))
         (struct-compress type ps)
         ps))
   (define (type-construct type vals)     
     (apply (struct-type-make type) vals))
   (define (type-deconstruct type val)    
     (struct->list type val))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (type-name self)))])

(define (struct->list type val)
  (let* ([getter (struct-type-ref type)]
         [vals (for/list ([i (struct-type-fields type)]) (getter val i))]
         [super (struct-type-super type)])
    (if super (append (struct->list super val) vals) vals)))


(define (merge*-all getter ps fields)
  (for/list ([i (in-range fields)])
    (apply merge* (for/list ([p ps])
                    (cons (car p) (getter (cdr p) i))))))

(define (struct-compress pred ps)
  (if (null? (cdr ps))
      ps
      (list (cons (apply || (map car ps))
                  (apply (struct-type-make pred)
                         (let loop ([type pred] [out '()])
                           (if type
                               (loop (struct-type-super type)
                                     (append (merge*-all (struct-type-ref type) 
                                                         ps
                                                         (struct-type-fields type)) 
                                             out))
                               out)))))))

; Assumes that (subtype? (get-type v) type) and (equal? type (get-type u)).      
(define (struct=? type u v =?)
  (let ([e+h (equal+hash type)])
    (if e+h 
        ((vector-ref e+h 0) u v =?)
        (and (struct-type-transparent? type) 
             (eq? type (get-type u))
             (eq? type (get-type v))
             (let loop ([type type])
               (or (false? type)
                   (let ([getter (struct-type-ref type)])
                     (and-&& 
                      (apply && (for/list ([i (struct-type-fields type)]) 
                                  (=? (getter u i) (getter v i))))
                      (loop (struct-type-super type))))))))))
                     
(define (equal+hash type) 
  (and type (or (struct-type-equal+hash type) 
                (equal+hash (struct-type-super type)))))

(define (least-common-super-struct-type type other)
  (let outer ([t0 type])
    (and t0 
         (or (let inner ([t1 other])
               (and t1 (or (and (eq? t0 t1) t0) 
                           (inner (struct-type-super t1)))))
             (outer (struct-type-super t0))))))
         
         
          

    
           


  

