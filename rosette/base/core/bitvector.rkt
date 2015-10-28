#lang racket

(require "term.rkt" "op.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" "merge.rkt" "safe.rkt")

(provide current-bitwidth 
         (rename-out [make-bv bv]) bv? 
         (rename-out [bitvector-type bitvector]) @bveq)

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
      [(union vs t)
       (and (subtype? self t)
            (match vs
              [(list _ ... (cons g (and (? typed?) (app get-type (== self)))) _ ...) g]
              [_ #f]))]
      [_ #f]))
  #:methods gen:type
  [(define (least-common-supertype self other) (if (equal? self other) self @any/c))
   (define (type-name self) (string->symbol (format "bitvector~a?" (bitvector-size self))))
   (define (type-applicable? self) #f)
   (define (cast self v)
     (match v
      [(bv _ (== self)) (values #t v)]
      [(term _ (== self)) (values #t v)]
      [(union vs t)
       (and (subtype? self t)
            (match vs
              [(list _ ... (cons gt (and (? typed? vt) (app get-type (== self)))) _ ...) (values gt vt)]
              [_ (values #f v)]))]
      [_ (values #f v)]))
   (define (type-eq? self u v)        (@bveq u v))
   (define (type-equal? self u v)     (@bveq u v))
   (define (type-compress self f? ps) ps)
   (define (type-construct self vs)   (car vs))
   (define (type-deconstruct self v)  (list v))]
  #:methods gen:custom-write
  [(define (write-proc self port m) 
     (fprintf port "(bitvector? ~a)" (bitvector-size self)))])


;; ----------------- Bitvector Literals ----------------- ;; 

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

; Returns a bitvector that best represents the given concrete number 
; with respect to the given precision specifier.  The specifier may 
; be either an exact-positive-integer? or a bitvector type. 
; The number may be a real, non-infinite, non-NaN concrete value.  
(define (make-bv val [precision (current-bitwidth)])
  (unless (and (real? val) (not (infinite? val)) (not (nan? val)))
    (raise-arguments-error 'bv "expected a real, non-infinite, non-NaN number" "value" val))
  (cond [(exact-positive-integer? precision) 
         (bv (sfinitize val precision) (bitvector-type precision))]
        [(bitvector? precision) 
         (bv (sfinitize val (bitvector-size precision)) precision)]
        [else 
         (raise-arguments-error 'bv "exact-positive-integer? or bitvector? type" "precision" precision)]))

;; ----------------- Lifting Procedures ----------------- ;;

(define (bitvector-type-error name . args)
  (arguments-error name "expected bitvectors of same length" "arguments" args))
  
(define (safe-apply-1 op x)
  (match x
    [(and (? typed? vx) (app get-type (? bitvector?))) (op x)]
    [(union xs _)
     (apply merge*
            (assert-some
             (let loop ([xs xs])
               (match xs
                 [(list) '()]
                 [(list (cons gx (and (? typed? vx) (app get-type (? bitvector?)))) rest ...)
                  (cons (cons gx (op vx)) (loop rest))]
                 [(list _ rest ...) (loop rest)]))
             #:unless (length xs)
             (bitvector-type-error (object-name op) x)))]
    [_ (bitvector-type-error (object-name op) x)]))

(define (safe-apply-2 op x y)
  (assert (and (typed? x) (typed? y)) (bitvector-type-error (object-name op) x y))
  (match* (x y)
    [((app get-type (? bitvector? tx)) _) 
     (if (eq? tx (get-type y))
         (op x y) 
         (op x (coerce y tx (object-name op))))]
    [(_ (app get-type (? bitvector? ty))) 
     (op (coerce x ty (object-name op)) y)]
    [((union xs _) (union ys _))
     (apply merge*
            (assert-some
             (let loop ([xs xs])
               (match xs
                 [(list) '()]
                 [(list (cons gx (and (? typed? vx) (app get-type (? bitvector? tx)))) rest ...)
                  (match ys
                    [(list _ ... (cons gy (and (? typed? vy) (app get-type (== tx)))) _ ...)
                     (match (&& gx gy)
                       [#f (loop rest)]
                       [g  (cons (cons g (op vx vy)) (loop rest))])])]
                 [(list _ rest ...)
                  (loop rest)]))
             #:unless (length xs)
             (bitvector-type-error (object-name op) x y)))]
    [(_ _) (bitvector-type-error (object-name op) x y)]))

(define (safe-apply-n op xs)
  (assert (for/and ([x xs]) (typed? x)) (apply bitvector-type-error (object-name op) xs))
  (match xs
    [(list _ ... (app get-type (? bitvector? t)) _ ...)
     (apply op (for/list ([x xs]) 
                 (if (eq? (get-type x) t) x (coerce x t (object-name op)))))]
    [(list (union vs _) (union ws _) ...)
     (apply merge*
            (assert-some
             (let loop ([vs vs])
               (match vs
                 [(list) '()]
                 [(list (cons gx (and (? typed? vx) (app get-type (? bitvector? tx)))) rest ...)
                  (match ws
                    [(list (list _ ... (cons gy (and (? typed? vy) (app get-type (== tx)))) _ ...) ...)
                     (match (apply && gx gy)
                       [#f (loop rest)]
                       [g  (cons (cons g (apply op vx vy)) (loop rest))])])]
                 [(list _ rest ...)
                  (loop rest)])))
            #:unless (length vs)
            (apply bitvector-type-error (object-name op) xs))]
    [_ (apply bitvector-type-error (object-name op) xs)]))
            
(define-syntax-rule (sort/expression op x y) 
  (if (term<? x y) 
      (expression op x y)
      (expression op y x)))

;; ----------------- Bitvector Operators ----------------- ;; 

(define (bveq x y) 
  (match* (x y)
    [((bv u _) (bv v _)) (= u v)]
    [((? bv?) (? term?)) (expression @bveq x y)]
    [((? term?) (? bv?)) (expression @bveq y x)]
    [(_ _) (sort/expression @bveq x y)]))

(define-operator @bveq 
  #:name 'bveq 
  #:type T*->boolean? 
  #:unsafe bveq
  #:safe (curry safe-apply-2 bveq))


