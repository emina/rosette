#lang racket


(require (for-syntax "../core/lift.rkt" racket/syntax)
         (only-in racket/private/generic-methods generic-property)
         (only-in "../core/effects.rkt" apply!)
         "../core/term.rkt"  "../core/lift.rkt" "../core/safe.rkt"
         (only-in "../core/bool.rkt" || && and-&&)
         (only-in "../core/type.rkt" @any/c type-cast gen:typed get-type)
         (only-in "../core/procedure.rkt" @procedure?)
         (only-in "../core/merge.rkt" merge merge*)
         (only-in "../core/union.rkt" union union? in-union-guards)
         (only-in "../core/equality.rkt" @equal? @eq?)
         (only-in "../adt/generic.rkt" adt-type-cast))

(provide @make-struct-type
         @make-struct-field-accessor
         @make-struct-field-mutator)

(define (@make-struct-type
         name super-type init-field-cnt auto-field-cnt
         [auto-v #f]
         [props '()]
         [inspector (current-inspector)]	 
         [proc-spec #f]
         [immutables '()]
         [guard #f]
         [constructor-name #f])

;  (printf "@make-struct-type:\n")
;  (printf " name: ~a\n" name)
;  (printf " super-type: ~a\n" super-type)
;  (printf " init-field-cnt: ~a\n" init-field-cnt)
;  (printf " auto-field-cnt: ~a\n" auto-field-cnt)
;  (printf " props: ~a\n" props)
;  (printf " inspector: ~a\n" inspector)
;  (printf " proc-spec: ~a\n" proc-spec)
;  (printf " immutables: ~a\n" immutables)
  
  (define-values (struct:t make-t t? t-ref t-set!)
    (make-struct-type
     name super-type init-field-cnt auto-field-cnt auto-v
     (cons (cons (generic-property gen:typed)
                 (vector (lambda (self) @struct:t)))
           props) ; all struct values are typed
     inspector proc-spec immutables
     guard constructor-name))
  
  (define (@t? v)
    (match v
      [(? t?) #t] 
      [(and (? typed? v) (app get-type t)) 
       (or (and t (subtype? t @struct:t)) 
           (and (union? v) (apply || (for/list ([g (in-union-guards v @struct:t)]) g))))]
      [_ #f]))

  (define super        (and super-type (typed? super-type) (get-type super-type)))
  (define field-count  (- init-field-cnt auto-field-cnt))
  (define immutable?   (and (= init-field-cnt (length immutables)) (zero? auto-field-cnt)))  
  (define transparent? (not inspector))
  (define equal+hash   (let ([e+h (assoc (generic-property gen:equal+hash) props)])
                         (and e+h (cdr e+h))))
  (define procedure?   (or proc-spec (not (false? (assoc prop:procedure props)))))

;  (printf " super: ~a\n" super)
;  (printf " field-count: ~a\n" field-count)
;  (printf " immutable?: ~a\n" immutable?)
;  (printf " transparent?: ~a\n" transparent?)
;  (printf " procedure?: ~a\n" procedure?)
;  (printf " equal+hash?: ~a\n" equal+hash?)
  
  (define @struct:t
    (struct-type 
     (procedure-rename @t? (object-name t?))
     super t? make-t t-ref t-set! field-count 
     (and immutable? (implies super (struct-type-immutable? super))) 
     (and transparent? (implies super (struct-type-transparent? super))) 
     (or procedure? (and super (struct-type-procedure? super)))
     equal+hash))
  
  (values struct:t make-t @struct:t t-ref t-set!))

(define (@make-struct-field-mutator struct:t i field-id)
  (let* ([@struct:t (get-type struct:t)]
         [native? (struct-type-native? @struct:t)]
         [setter (make-struct-field-mutator (struct-type-set! @struct:t) i field-id)]
         [getter (make-struct-field-accessor (struct-type-ref @struct:t) i field-id)])
    (procedure-rename 
     (lambda (receiver value)  
       (if (native? receiver)
           (apply! setter getter receiver value) 
           (match (type-cast @struct:t receiver (object-name setter))
             [(? native? r) (apply! setter getter r value)]
             [(union rs) (for ([r rs]) 
                           (apply! setter getter (cdr r) (merge (car r) value (getter (cdr r)))))])))
     (object-name setter))))

(define (@make-struct-field-accessor struct:t i field-id)
  (let* ([@struct:t (get-type struct:t)]
         [native? (struct-type-native? @struct:t)]
         [getter (make-struct-field-accessor (struct-type-ref @struct:t) i field-id)])
    (procedure-rename 
     (lambda (receiver) 
       (if (native? receiver)
           (getter receiver)
           (match (type-cast @struct:t receiver (object-name getter))
             [(? native? r) (getter r)]
             [(union r) (merge** r getter)])))
     (object-name getter))))

(struct struct-type (pred super native? make ref set! fields immutable? transparent? procedure? equal+hash)
  #:property prop:procedure
  [struct-field-index pred]
  #:methods gen:type
  [(define (least-common-supertype type other)
     (or (and (eq? type other) type)
         (and (eq? other @procedure?) (struct-type-procedure? type) @procedure?)
         (and (not (struct-type? other)) @any/c)
         (least-common-super-struct-type type other)
         (and (struct-type-procedure? type) (struct-type-procedure? other) @procedure?)
         @any/c))
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
   (define (type-cast self v [caller 'type-cast])
     (adt-type-cast v #:type (struct-type-native? self) #:lifted self #:caller caller)) 
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
         
         

    
           


  

