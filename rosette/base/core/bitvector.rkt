#lang racket

(require "term.rkt" "op.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" "merge.rkt" "safe.rkt")

(provide 
 current-bitwidth 
 (rename-out [@bv bv]) bv? 
 (rename-out [bitvector-type bitvector]) bitvector-size bitvector? 
 ; lifted versions of the operators
 @bveq @bvnot @bvor @bvand)

;; ----------------- Bitvector Types ----------------- ;; 

; Cache of all bitvector types constructed so far, mapping sizes to types.
(define bitvector-types (make-hash))

; Returns the bitvector type of the given size.
(define (bitvector-type [size (current-bitwidth)])
  (unless (exact-positive-integer? size)
    (raise-argument-error 'bitvector "exact-positive-integer?" size))
  (or (hash-ref bitvector-types size #f)
      (let ([t (bitvector size)]) 
        (hash-set! bitvector-types size t)
        t)))

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
   (define (type-compress self f? ps) (generic-merge bvor (bv 0 self) ps))
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
   5 
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

; Pattern matching for bitvector literals.
(define-match-expander @bv
  (syntax-rules ()
    [(_ val-pat type-pat) (bv val-pat type-pat)])
  (syntax-id-rules (set!)
    [(@bv v t) (make-bv v t)]
    [@bv make-bv]))

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
     (if (equal? tx (get-type y))
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
                 (if (equal? (get-type x) t) x (coerce x t (object-name op)))))]
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

;; ----------------- Bitvector Comparison Operators ----------------- ;; 

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

;; ----------------- Bitvector Bitwise Operators ----------------- ;;

(define (bvnot x)
  (match x
    [(bv v t) (bv (bitwise-not v) t)]
    [(expression (== @bvnot) v) v]
    [_ (expression @bvnot x)]))

(define-operator @bvnot
  #:name 'bvnot
  #:type T*->T
  #:unsafe bvnot
  #:safe (lambda (x) (safe-apply-1 bvnot x)))

(define bvand (bitwise-connective bitwise-and @bvand @bvor -1 0))

(define-operator @bvand
  #:name 'bvand
  #:type T*->T
  #:unsafe bvand
  #:safe (case-lambda [() (bvand)]
                      [(x) (safe-apply-1 bvand x)]
                      [(x y) (safe-apply-2 bvand x y)]
                      [xs (safe-apply-n bvand xs)]))

(define bvor (bitwise-connective bitwise-ior @bvor @bvand 0 -1))

(define-operator @bvor
  #:name 'bvor
  #:type T*->T
  #:unsafe bvand
  #:safe (case-lambda [() (bvor)]
                      [(x) (safe-apply-1 bvor x)]
                      [(x y) (safe-apply-2 bvor x y)]
                      [xs (safe-apply-n bvor xs)]))
    
; Partial evaluation rules for bvand and bvor.  The 
; terms iden and !iden should be literals.
(define-syntax-rule (bitwise-connective racket-op op co iden !iden)
  (case-lambda 
    [() (make-bv iden)]
    [(x) x]
    [(x y) 
     (match* (x y)
       [((bv u t) (bv v _)) (bv (racket-op u v) t)]
       [((bv iden _) _) y]
       [(_ (bv iden _)) x]
       [((bv !iden _) _) x]
       [(_ (bv !iden _)) y]
       [(_ _)
        (or
         (simplify-connective op co (bv !iden (get-type x)) x y)
         (cond [(bv? x) (expression op x y)]
               [(bv? y) (expression op y x)]
               [else    (sort/expression op x y)]))])] 
    [xs 
     (let*-values ([(lits terms) (partition bv? xs)]
                   [(lit) (for/fold ([out iden]) ([lit lits])
                            (racket-op out (bv-value lit)))]
                   [(t) (get-type (car xs))])
       (if (or (= lit !iden) (null? terms)) 
           (bv lit t)
           (match (simplify-connective* op co (bv !iden t) (remove-duplicates terms))
             [(list (bv u _)) (bv (racket-op lit u) t)]
             [(list y) (if (= lit iden) y (expression op lit y))]
             [ys (if (= lit iden)
                     (apply expression op (sort ys term<?))
                     (apply expression op (bv lit t) (sort ys term<?)))])))]))

(define (simplify-connective op co !iden x y) 
  (cond [(equal? x y) x]
        [(expression? x)
         (cond [(expression? y)
                (or (simplify-connective:expr/term op co !iden x y)
                    (simplify-connective:expr/term op co !iden y x)
                    (match* (x y)
                      [((expression (== op) xs ...) (expression (== op) ys ...))
                       (for*/or ([a xs][b ys])
                         (match* (a b)
                           [(_ (expression (== @bvnot) (== a))) !iden]
                           [((expression (== @bvnot) (== b)) _) !iden]
                           [((bv x _) (bv y _)) (and (= x (bitwise-not y)) !iden)]
                           [(_ _) #f]))]
                      [(_ _) #f]))]
               [(constant? y) (simplify-connective:expr/term op co !iden x y)]
               [else (simplify-connective:expr/lit op co !iden x y)])]
        [(expression? y)
         (cond [(constant? x) (simplify-connective:expr/term op co !iden y x)]
               [else (simplify-connective:expr/lit op co !iden y x)])]
        [else #f]))

(define (simplify-connective:expr/term op co !iden x y)
  (match x 
    [(expression (== @bvnot) (== y)) !iden]
    [(expression (== co) _ ... (== y) _ ...) y]
    [(expression (== op) _ ... (== y) _ ...) x]
    [(expression (== op) _ ... (expression (== @bvnot) (== y)) _ ...) !iden]
    [(expression (== @bvnot) (expression (== co) _ ... (== y) _ ...)) !iden]
    [(expression (== @bvnot) (expression (== co) _ ... (expression (== @bvnot) (== y)) _ ...)) x]
    [(expression (== @bvnot) (expression (== op) _ ... (expression (== @bvnot) (== y)) _ ...)) y]
    [(expression (== @bvnot) a) 
     (match y 
       [(expression (== op) _ ... (== a) _ ...) !iden]
       [_ #f])]
    [_ #f]))

(define (simplify-connective:expr/lit op co !iden x y)
  (define !y (bvnot y))
  (match x 
    [(expression (== co) (== y) _ ...) y]
    [(expression (== op) (== y) _ ...) x]
    [(expression (== op) (== !y) _ ...) !iden]
    [(expression (== @bvnot) (expression (== co) (== y) _ ...)) !iden]
    [(expression (== @bvnot) (expression (== co) (== !y) _ ...)) x]
    [(expression (== @bvnot) (expression (== op) (== !y) _ ...)) y]
    [_ #f]))

(define (simplify-connective* op co !iden xs)
  (or
   (let-values ([(!ys ys) (for/fold ([!ys '()][ys '()]) ([x xs])
                            (match x
                              [(expression (== @bvnot) y) (values (cons y !ys) ys)]
                              [_ (values !ys (cons x ys))]))])
     (for/first ([!y !ys] #:when (member !y ys)) (list !iden)))
   (and (> (length xs) 100) xs)
   (let outer ([xs xs])
     (match xs
       [(list x rest ..1)
        (let inner ([head rest] [tail '()])
          (match head
            [(list) (match (outer tail)
                      [(and (list (== !iden)) t) t]
                      [t (cons x t)])]
            [(list y ys ...)
             (match (simplify-connective op co !iden x y)
               [#f (inner ys (cons y tail))]
               [(== !iden) (list !iden)]
               [v (outer (cons v (append ys tail)))])]))]
       [_ xs]))))

;; ----------------- Bitvector Arithmetic Operators ----------------- ;;


(require "../form/define.rkt")
(define bv4 (bitvector-type 4))
(define-symbolic x y z bv4)