#lang racket

(require (for-syntax racket/syntax) racket/stxparam racket/stxparam-exptime)
(require "term.rkt" "op.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" "merge.rkt" "safe.rkt")

(provide 
 current-bitwidth 
 (rename-out [@bv bv]) bv? 
 (rename-out [bitvector-type bitvector]) bitvector-size bitvector? 
 ; lifted versions of the operators
 @bveq @bvnot @bvor @bvand @bvxor @bvneg @bvadd @bvsub)

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
    [(@bv v) (make-bv v)]
    [@bv make-bv]))


;; ----------------- Lifitng Utilities ----------------- ;;

(define (lift-op op)
  (case (procedure-arity op)
    [(1)  (lambda (x) (safe-apply-1 op x))]
    [(2)  (lambda (x y) (safe-apply-2 op x y))]
    [else (case-lambda [() (op)]
                       [(x) (safe-apply-1 op x)]
                       [(x y) (safe-apply-2 op x y)]
                       [xs (safe-apply-n op xs)])]))

(define (sort/expression @bvop x y) 
  (cond [(bv? x) (expression @bvop x y)]
        [(bv? y) (expression @bvop y x)]
        [(term<? x y) (expression @bvop x y)]
        [else (expression @bvop y x)]))

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
    [_ (assert #f (bitvector-type-error (object-name op) x))]))

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
    [(_ _) (assert #f (bitvector-type-error (object-name op) x y))]))

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
    [_ (assert #f (apply bitvector-type-error (object-name op) xs))]))

(define-syntax-parameter finitize
  (syntax-rules () [(_ e t) e]))

(define-syntax-rule (define-lifted-operator @bvop bvop type)
  (define-operator @bvop
    #:name 'bvop
    #:type type
    #:unsafe bvop
    #:safe (lift-op bvop)))

;; ----------------- Bitvector Comparison Operators ----------------- ;; 

(define (bveq x y) 
  (match* (x y)
    [((bv u _) (bv v _)) (= u v)]
    [(_ _) (sort/expression @bveq x y)]))

(define-lifted-operator @bveq bveq T*->boolean?)

;; ----------------- Bitvector Bitwise Operators ----------------- ;;

(define bvnot (bitwise-negation bitwise-not bvnot @bvnot))
(define-lifted-operator @bvnot bvnot T*->T)

(define bvand (bitwise-connective bitwise-and bvand @bvand @bvor -1 0))
(define-lifted-operator @bvand bvand T*->T)

(define bvor (bitwise-connective bitwise-ior bvor @bvor @bvand 0 -1))
(define-lifted-operator @bvor bvor T*->T)

(define bvxor (bitwise-adder bitwise-xor bvxor @bvxor simplify-bvxor))
(define-lifted-operator @bvxor bvxor T*->T)

; Simplification rules for bvxor.
(define (simplify-bvxor x y)
  (match* (x y)
    [((bv u t) (bv v _)) (bv (bitwise-xor u v) t)]
    [(_ (== x)) (bv 0 (get-type x))]
    [(_ (bv 0 _)) x]
    [((bv 0 _) _) y]
    [(_ (bv -1 _)) (@bvnot x)]
    [((bv -1 _) _) (@bvnot y)]
    [(_ (expression (== @bvnot) (== x))) (bv -1 (get-type x))]
    [((expression (== @bvnot) (== y)) _) (bv -1 (get-type x))]
    [(_ _) #f]))

;; ----------------- Bitvector Arithmetic Operators ----------------- ;;

(define-values (bvneg bvadd) 
  (syntax-parameterize 
   ([finitize (syntax-rules () [(_ e t) (sfinitize e (bitvector-size t))])]) 
   (values (bitwise-negation - bvneg @bvneg)
           (bitwise-adder + bvadd @bvadd simplify-bvadd))))

(define bvsub 
  (case-lambda [(x) (bvneg x)]
               [(x y) (bvadd x (bvneg y))]
               [(x . xs) (apply bvadd x (map bvneg xs))]))

(define-lifted-operator @bvneg bvneg T*->T)
(define-lifted-operator @bvadd bvadd T*->T)
(define-lifted-operator @bvsub bvsub T*->T)


; Simplification rules for bvadd.
(define (simplify-bvadd x y)
  (cond [(and (bv? x) (bv? y)) 
         (let ([t (get-type x)])
           (bv (sfinitize (+ (bv-value x) (bv-value y)) (bitvector-size t)) t))]
        [(and (bv? x) (zero? (bv-value x))) y]
        [(and (bv? y) (zero? (bv-value y))) x]
        [(expression? x)
         (or (simplify-bvadd:expr/term x y)
             (and (expression? y) 
                  (simplify-bvadd:expr/term y x)))]
        [(expression? y)
         (simplify-bvadd:expr/term y x)]
        [else #f]))
                
(define (simplify-bvadd:expr/term x y)
  (match* (x y) 
    [((expression (== @bvneg) (== y)) _) (bv 0 (get-type x))]
    [((expression (== @bvneg) (expression (== @bvadd) (== y) z)) _) (bvneg z)]
    [((expression (== @bvneg) (expression (== @bvadd) z (== y))) _) (bvneg z)]
    [((expression (== @bvadd) (expression (== @bvneg) (== y)) z) _) z]
    [((expression (== @bvadd) z (expression (== @bvneg) (== y))) _) z]
    [((expression (== @bvadd) (bv a _) b) (bv (app - a) _)) b]
    [((expression (== @bvadd) a b) (expression (== @bvneg) a)) b]
    [((expression (== @bvadd) a b) (expression (== @bvneg) b)) a]
    [((expression (== @bvadd) a ...) (expression (== @bvadd) b ...))
     (let ([alen (length a)] 
           [blen (length b)])
       (and (<= (abs (- alen blen)) 1)
            (let*-values ([(a b) (if (<= alen blen) (values a b) (values b a))])
              (match (remove* (map bvneg a) b)
                [(list) (bv 0 (get-type x))]
                [(list c) c]
                [_ #f]))))] 
    [(_ _) #f]))
    
;; ----------------- Shared lifting procedures and templates ----------------- ;;

; Partial rules for negators (bvnot and bvneg).
(define-syntax-rule (bitwise-negation op bvop @bvop)
  (lambda (x)
    (match x
      [(bv v t) (bv (finitize (op v) t) t)]
      [(expression (== @bvop) v) v]
      [_ (expression @bvop x)])))
  
; Partial evaluation rules for connectives (bvand and bvor).  
; The terms iden and !iden should be numeric literals.
(define-syntax-rule (bitwise-connective op bvop @bvop @bvco iden !iden)
  (case-lambda 
    [() (make-bv iden)]
    [(x) x]
    [(x y) 
     (match* (x y)
       [((bv u t) (bv v _)) (bv (op u v) t)]
       [((bv iden _) _) y]
       [(_ (bv iden _)) x]
       [((bv !iden _) _) x]
       [(_ (bv !iden _)) y]
       [(_ _)
        (or
         (simplify-connective @bvop @bvco (bv !iden (get-type x)) x y)
         (sort/expression @bvop x y))])]
    [xs 
     (let*-values ([(lits terms) (partition bv? xs)]
                   [(lit) (for/fold ([out iden]) ([lit lits])
                            (op out (bv-value lit)))]
                   [(t) (get-type (car xs))])
       (if (or (= lit !iden) (null? terms)) 
           (bv lit t)
           (match (simplify-connective* @bvop @bvco (bv !iden t) (remove-duplicates terms))
             [(list (bv u _)) (bv (op lit u) t)]
             [(list y) (bvop (bv lit t) y)]
             [ys (if (= lit iden)
                     (apply expression @bvop (sort ys term<?))
                     (apply expression @bvop (bv lit t) (sort ys term<?)))])))]))

; Partial evaluation rules for adders (bvxor and bvadd).
(define-syntax-rule (bitwise-adder op bvop @bvop simplify-bvop)
  (case-lambda 
    [() (@bv 0)]
    [(x) x]
    [(x y) (or (simplify-bvop x y)
               (sort/expression @bvop x y))]
    [xs (let*-values ([(lits terms) (partition bv? xs)]
                      [(lit) (for/fold ([out 0]) ([lit lits]) (op out (bv-value lit)))]
                      [(t) (get-type (car xs))])
          (if (null? terms)
              (bv (finitize lit t) t)
              (match (simplify-op* (if (null? lits) 
                                       terms 
                                       (cons (bv (finitize lit t) t) terms)) 
                                   simplify-bvop)
                [(list y) y]
                [(list a (... ...) (? bv? b) c (... ...)) 
                 (apply expression @bvop b (sort (append a c) term<?))]
                [ys (apply expression @bvop (sort ys term<?))])))]))

; Simplification rules for bvand and bvor.  The 
; terms iden and !iden should be bitvector literals.
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
                      [((expression (== co) xs ...) (expression (== co) ys ...))
                       (cond [(sublist? xs ys) x]
                             [(sublist? ys xs) y]
                             [else #f])]                      
                    [(_ _) #f]))]
               [(constant? y) (simplify-connective:expr/term op co !iden x y)]
               [else (simplify-connective:expr/lit op co !iden x y)])]
        [(expression? y)
         (cond [(constant? x) (simplify-connective:expr/term op co !iden y x)]
               [else (simplify-connective:expr/lit op co !iden y x)])]
        [else #f]))


; Returns #t if ys contains all elements of xs, in the order 
; in which they occur in xs. Otherwise returns #f.
(define (sublist? xs ys)
  (and (<= (length xs) (length ys))
       (match xs
         [(list) #t]
         [(list x xs ...)
          (match ys 
            [(list _ ... (== x) ys ...) (sublist? xs ys)]
            [_ #f])])))

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


(define (simplify-op* xs simplify-op)
  (or
   (and (> (length xs) 100) xs)
   (let ([out (let outer ([xs xs])
                (match xs
                  [(list x rest ..1)
                   (let inner ([head rest] [tail '()])
                     (match head
                       [(list) (cons x (outer tail))]
                       [(list y ys ...)
                        (match (simplify-op x y)
                          [#f (inner ys (cons y tail))]
                          [v (outer (cons v (append ys tail)))])]))]
                  [_ xs]))])
     (if (= (length out) (length xs)) out (simplify-op* out simplify-op)))))            





;(require "../form/define.rkt")
;(define bv4 (bitvector-type 4))
;(define-symbolic x y z bv4)