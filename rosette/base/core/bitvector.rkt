#lang racket

(require "term.rkt" "op.rkt" "union.rkt" "bool.rkt" "generic.rkt" "merge.rkt")

(provide current-bitwidth 
         (rename-out [make-bv bv]) bv?)

;; ----------------- Bitvector Literals ----------------- ;; 

; Returns a bitvector literal that best represents the given real, non-infinite, non-NaN number, 
; with respect to the given precision specifier.  The specifier may be either an exact-positive-integer? 
; or a bitvector type.
(define (make-bv val [precision (current-bitwidth)])
  (unless (and (real? val) (not (infinite? val)) (not (nan? val)))
    (raise-arguments-error 'bv "expected a real, non-infinite, non-NaN number" "value" val))
  (cond [(exact-positive-integer? precision) 
         (bv (sfinitize val precision) (bitvector-type precision))]
        [(bitvector? precision) 
         (bv (sfinitize val (bitvector-size precision)) precision)]
        [else 
         (raise-arguments-error 'bv "exact-positive-integer? or bitvector? type" "precision" precision)]))

; Represents a bitvector literal.
(struct bv (value type)
  #:transparent
  #:methods gen:typed
  [(define (get-type self) (bv-type self))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "(bv ~a ~a)" 
              (bv-value self)
              (bitvector-size (bv-type self))))])

; Parameter that controls the bitwidth of all bitvector literals for which a precision is 
; not explicitly specified.
(define current-bitwidth
  (make-parameter 
   8 
   (lambda (bw) 
     (unless (exact-positive-integer? bw)
       (raise-argument-error 'current-bitwidth "exact-positive-integer?" bw))
     bw)))

; Returns a signed representation of the given number, using the specified bitwidth.
; Assumes that val is a real, non-infinite, non-NaN number.
(define (sfinitize val bitwidth) 
  (let* ([mask (arithmetic-shift -1 bitwidth)]
         [masked (bitwise-and (bitwise-not mask) (exact-truncate val))])
    (if (bitwise-bit-set? masked (- bitwidth 1))
        (bitwise-ior mask masked)  
        masked)))

; Returns an unsigned representation of the given number, using the specified bitwidth.
; Assumes that val is a real, non-infinite, non-NaN number.
(define (ufinitize val bitwidth) 
  (let* ([mask (arithmetic-shift -1 bitwidth)]
         [masked (bitwise-and (bitwise-not mask) (exact-truncate val))])
    masked))

;; ----------------- Bitvector Types ----------------- ;; 

; Cache of all bitvector types constructed so far, mapping sizes to types.
(define bitvector-types (make-hash))

; Returns the bitvector type of the given size.
(define (bitvector-type [size (current-bitwidth)])
  (unless (exact-positive-integer? size)
    (raise-argument-error 'bitvector "exact-positive-integer?" size))
  (or (hash-ref bitvector-types size #f)
      (hash-ref! bitvector-types size (bitvector size))))

; Represents a bitvector type.
(struct bitvector (size)
  #:transparent
  #:property prop:procedure ; Recognizes bitvector values of this type.
  (lambda (self v)
    (match v
      [(bv _ (== self)) #t]
      [(term _ (== self)) #t]
      [(union : [g (and (? typed?) (app get-type (== self)))] _ ...) g]
      [_ #f]))
  #:methods gen:type
  [(define (least-common-supertype self other) (if (equal? self other) self @any/c))
   (define (type-name self) (string->symbol (format "bitvector~a?" (bitvector-size self))))
   (define (type-applicable? self) #f)
   (define (cast self v)
     (match v
      [(bv _ (== self)) (values #t v)]
      [(term _ (== self)) (values #t v)]
      [(union : [g (and (? typed?) (app get-type (== self)) b)] _ ...) (values g b)]
      [_ (values #f v)]))
   (define (type-eq? self u v)        (@bveq u v))
   (define (type-equal? self u v)     (@bveq u v))
   (define (type-compress self f? ps) ps)
   (define (type-construct self vs)   (car vs))
   (define (type-deconstruct self v)  (list v))]
  #:methods gen:custom-write
  [(define (write-proc self port m) 
     (fprintf port "(bitvector? ~a)" (bitvector-size self)))])

(define binary-predicate-type (op/-> (@any/c @any/c) @boolean?))
(define (args-of-same-bitvector-type . xs) (bitvector? (apply type-of xs)))

(define-syntax-rule (sort/expression op x y) 
  (if (term<? x y) 
      (expression op x y)
      (expression op y x)))

(define-op @bveq 
  #:name 'bveq
  #:type binary-predicate-type
  #:pre  args-of-same-bitvector-type
  #:op (lambda (x y) 
         (match* (x y)
           [((bv u _) (bv v _)) (= u v)]
           [((? bv?) (? term?)) (expression @bveq x y)]
           [((? term?) (? bv?)) (expression @bveq y x)]
           [(_ _) (sort/expression @bveq x y)])))
           





