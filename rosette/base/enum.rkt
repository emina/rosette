#lang racket

(require (for-syntax racket/syntax)  
         (only-in "safe.rkt" coerce type-error argument-error assert-some)
         (only-in "lift.rkt" merge**)
         "term.rkt" "op.rkt" 
         (only-in "bool.rkt" @boolean? || and-&&)
         (only-in "num.rkt" @number?)
         (only-in "any.rkt" @any?)
         (only-in "merge.rkt" merge*)
         (only-in "union.rkt" union union? in-union* in-union-guards union-filter union-guards)
         (only-in "equality.rkt" @equal?)
         (only-in "generic.rkt" do-cast =?))

(provide define-enum enums enum? enum-size enum-members enum-<? label ordinal
         enum-first enum-last enum-value [rename-out (atom? enum-literal?)])

; A list of all enum types created so far.
(define enums '())

; Defines a new enumerated type, consisting of fresh values.  
; Each enum value has a label and an ordinal that specifies 
; its position in the enumerated type. The labels for the values, 
; and their ordinals, correspond to the list of labels with which 
; the enum is defined. The form will fail to create a new enum type 
; if the labels list is empty or contains duplicates. 
;
; The (define-enum id labels) form introduces three identifiers:  
; id, id? and id<?.  The id identifier is bound to the accessor 
; procedure that takes as input a label and returns the corresponding 
; enum value; id? is bound to the created enum? type; and id<?  
; is a comparator procedure that compares (the ordinals of) enum 
; values of type id?.   
(define-syntax (define-enum stx)
  (syntax-case stx ()
    [(_ id labels)         
     (with-syntax ([id? (format-id #'id "~a?" #'id #:source #'id)]
                   [id<? (format-id #'id "~a<?" #'id #:source #'id)])
       (syntax/loc stx 
         (begin 
           (define id?  (make-enum 'id 'id<? labels))
           (define id   (enum-member id?))
           (define id<? (enum-<? id?)))))])) 

; A member of an enum type is an atom with an index, a label and a type.
; Two atoms are equal iff they are the same object.  Note that this
; works because the enum type constructor (enum-member) doesn't 
; construct any new objects when given a label; it simply returns 
; the atom from its cache that corresponds to the given label.
(struct atom (index label type)
  #:methods gen:typed
  [(define (get-type self) (atom-type self))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(~a ~a)" 
              (object-name (enum-member (atom-type self)))
              (atom-label self)))])

; An enum type has a vector of member atoms, a procedure that takes 
; in a label and returns the corresponding atom, and an operator for 
; comparing two values of this enum type, if the type is ordered.
(struct enum (members member <?)
  #:mutable
  #:property prop:procedure
  (lambda (self v)
    (match v
      [(atom _ _ (== self)) #t]
      [(term  _ (== self)) #t]
      [(union  _ (== self)) #t]
      [(union vs (== @any?)) (apply || (for/list ([g (in-union-guards vs self)]) g))]
      [_ #f]))
  #:methods gen:type
  [(define (least-common-supertype t other) (if (eq? t other) t @any?))
   (define (type-name t)      (string->symbol (~a (object-name (enum-member t)) '?)))
   (define (type-applicable? t) #f)
   (define (cast t v)              
     (match v
       [(atom _ _ (== t)) (values #t v)]
       [(term  _ (== t)) (values #t v)]
       [(union  _ (== t)) (values #t v)]
       [(union vs (== @any?))
        (match (union-filter v t)
          [(union (list (cons g u))) (values g u)]
          [r (values (apply || (union-guards r)) r)])]
       [_ (values #f v)]))
   (define (type-eq? t u v)        (=? u v))
   (define (type-equal? t u v)     (=? u v))                                                       
   (define (type-compress t f? ps) ps)
   (define (type-construct t vs)   (car vs))
   (define (type-deconstruct t v)  (list v))]
  #:methods gen:custom-write
  [(define (write-proc self p m)   (fprintf p "~a?" (object-name (enum-member self))))])

; Given an enum and a concrete or symbolic label, returns the enum 
; member with that label.  If no such member exists, an error is thrown.
(define (enum-value t label) ((enum-member t) label))

; Returns the first member of the given enum type.
(define (enum-first t) (vector-ref (enum-members t) 0))

; Returns the last member of the given enum type.
(define (enum-last t)
  (let ([members (enum-members t)]) 
    (vector-ref members (sub1 (vector-length members)))))

; Given an enum, returns its size, given as the number of 
; its members.
(define (enum-size t) (vector-length (enum-members t)))

; Given a concrete or symbolic enum member, returns its label.
(define (label v)
  (match v
    [(atom _ l _) l]
    [(term _ (enum members _ _)) 
     (apply merge* (for/list ([m members]) (cons (@equal? v m) (atom-label m))))]
    [(union vs (? enum?)) (merge** vs label)]
    [(union _ (== @any?))
     (apply merge* (assert-some 
                  (for/list ([(g v) (in-union* v)] #:when (enum? (type-of v)))
                    (cons g (label v)))
                  (type-error 'label enum? v)))]
    [_ (raise-argument-error 'label "enum?" v)]))

; Given a concrete or symbolic value of type t, where t is an enum? type,
; returns the index of that value in (enum-members t).
(define (ordinal v)
  (match v
    [(atom idx _ _) idx]
    [(term _ (enum members _ _)) 
     (apply merge* (for/list ([m members]) (cons (@equal? v m) (atom-index m))))]
    [(union vs (? enum?)) (merge** vs ordinal)]
    [(union _ (== @any?))
     (apply merge* (assert-some 
                  (for/list ([(g v) (in-union* v)] #:when (enum? (type-of v)))
                    (cons g (ordinal v)))
                  (type-error 'ordinal enum? v)))]
    [_ (raise-argument-error 'ordinal "enum?" v)]))

;;; Helpers ;;; 

; Makes a new enum using the given base name, 
; comparator name, and labels.
(define (make-enum id id<? labels) 
  (define t (make<? id<? (make-members id labels)))
  (set! enums (cons t enums))
  t)
               
               
; Returns a new enum type, and initializes it with fresh atoms and a 
; member procedure.  One atom is created for each label, and the atoms 
; are ordered according to the order of labels.  Throws an error if the 
; labels list is empty or contains duplicates. 
(define (make-members id labels)
  (when (null? labels)
    (error 'define-enum "expected a non-empty set of enum labels for ~a" id))

  (define t (enum #f #f #f))
  
  (define members (apply vector-immutable 
                         (for/list ([(l i) (in-indexed labels)])
                           (atom i l t)))) 
  
  (define label->member (for/hash ([l labels] [m members]) 
                          (values l m)))
  
  (unless (= (vector-length members) (hash-count label->member))
    (error 'define-enum "expected unique enum labels for ~a" id))
  
  (set-enum-members! t members)
  (set-enum-member!  t (make-member id label->member))
  t)

; Returns the member procedure for the given table from labels to atoms.
(define (make-member id label->member)
  (procedure-rename
   (lambda (label)
     (or (hash-ref label->member label #f)
         (apply merge* (assert-some (for/fold ([gv '()]) ([(l m) label->member])
                                    (match (@equal? l label)
                                      [#f gv]
                                      [g (cons (cons g m) gv)]))
                                  (argument-error id (format "a ~a label" id) label)))))
   id))


; Initializes the given enum type with an enum comparison 
; operator that compares atoms according to their index.  
; Returns the initialized type.
(define (make<? id t)
  (define-op enum<?
    #:name id
    #:type (op/-> (t t) @boolean?)
    #:op   
    (lambda (x y)
      (match* ((coerce x t id) (coerce y t id))
        [((atom i _ _) (atom j _ _)) (< i j)]
        [((? union? x) (? union? y))
         (apply || (for*/list ([(gx vx) (in-union* x)] [(gy vy) (in-union* y)])
                     (and-&& (enum<? vx vy) gx gy)))]
        [((? union? x) y)
         (apply || (for/list ([(gx vx) (in-union* x)]) 
                     (and-&& (enum<? vx y) gx)))]
        [(x (? union? y))
         (apply || (for/list ([(gy vy) (in-union* y)])
                     (and-&& (enum<? x vy) gy)))]
        [(x y) (expression enum<? x y)])))
  (set-enum-<?! t enum<?)
  t)

#|
(require (only-in "control.rkt" @if))
(define-enum foo '(1 2 3 4))
(define x (constant #'x foo?))
(define y (constant #'y foo?))
(label x)
(label (@if (@equal? x y) x (foo 1)))
(ordinal x)
(ordinal (@if (@equal? x y) x (foo 1)))
|#
