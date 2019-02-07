#lang racket

(require racket/stxparam racket/stxparam-exptime
         (for-syntax racket/syntax syntax/transformer))
(require "term.rkt" "union.rkt" "bool.rkt" "polymorphic.rkt" 
         "merge.rkt" "safe.rkt" "lift.rkt" "forall.rkt")
(require (only-in "real.rkt" @>= @> @= @integer? T*->integer?))

(provide 
 (rename-out [@bv bv]) @bv? bv? bv-value bv-type
 (rename-out [@bitvector bitvector]) bitvector-size bitvector? 
 @bveq @bvslt @bvsgt @bvsle @bvsge @bvult @bvugt @bvule @bvuge
 @bvnot @bvor @bvand @bvxor @bvshl @bvlshr @bvashr
 @bvneg @bvadd @bvsub @bvmul @bvudiv @bvsdiv @bvurem @bvsrem @bvsmod
 @concat @extract @sign-extend @zero-extend @integer->bitvector @bitvector->integer @bitvector->natural)

;; ----------------- Bitvector Types ----------------- ;; 

; Cache of all bitvector types constructed so far, mapping sizes to types.
(define bitvector-types (make-hash))

; Returns the bitvector type of the given size.
(define (bitvector-type size)
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
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(bv _ (== self)) v]
       [(term _ (== self)) v]
       [(union (list _ ... (cons gt (and (? typed? vt) (app get-type (== self)))) _ ...) _) 
        (assert gt (thunk (error caller "expected ~a, given ~.a" self v)))
        vt]
       [_ (assert #f (thunk (error caller "expected ~a, given ~.a" self v)))]))
   (define (type-eq? self u v)        (@bveq u v))
   (define (type-equal? self u v)     (@bveq u v))
   (define (type-compress self f? ps) (generic-merge* ps))
   (define (type-construct self vs)   (car vs))
   (define (type-deconstruct self v)  (list v))]
  #:methods gen:solvable
  [(define (solvable-default self) (bv 0 self))
   (define (solvable-domain self) null)
   (define (solvable-range self) self)]
  #:methods gen:custom-write
  [(define (write-proc self port m) 
     (fprintf port "(bitvector ~a)" (bitvector-size self)))])

; Pattern matching for bitvector types.
(define-match-expander @bitvector
  (syntax-rules ()
    [(_ sz) (bitvector sz)])
  (make-variable-like-transformer #'bitvector-type))

(define (bvsmin t) (- (expt 2 (- (bitvector-size t) 1))))
(define (bvsmin? b) (and (bv? b) (= (bv-value b) (bvsmin (bv-type b)))))
(define (bvsmax t) (- (expt 2 (- (bitvector-size t) 1)) 1))
(define (bvsmax? b) (and (bv? b) (= (bv-value b) (bvsmax (bv-type b)))))
(define (is-bitvector? v) (and (typed? v) (bitvector? (get-type v))))

;; ----------------- Bitvector Literals ----------------- ;; 

; Represents a bitvector literal.
(struct bv (value type)
  #:transparent
  #:methods gen:typed
  [(define (get-type self) (bv-type self))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match self
       [(bv v (bitvector bw))
        (let*-values ([(q r) (quotient/remainder bw 4)]
                      [(p b mw) (if (zero? r) (values "x" 16 q) (values "b" 2 bw))])
            (fprintf port "(bv #~a~a ~a)"
                     p
                     (~r (ufinitize v bw) #:base b #:pad-string "0" #:min-width mw)
                     bw))]))])

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
(define (make-bv val precision)
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
  (make-variable-like-transformer #'make-bv))

(define (@bv? v)
  (match v
    [(? bv?) #t]
    [(term _ (? bitvector?)) #t]
    [(union _ (? bitvector?)) #t]
    [(union xs (== @any/c))
     (apply || (for/list ([gv xs] #:when (@bv? (cdr gv))) (car gv)))]
    [_ #f]))


;; ----------------- Lifitng Utilities ----------------- ;;

(define (lift-op op)
  (case (procedure-arity op)
    [(1)  (lambda (x) (safe-apply-1 op x))]
    [(2)  (lambda (x y) (safe-apply-2 op x y))]
    [else (case-lambda [(x) (safe-apply-1 op x)]
                       [(x y) (safe-apply-2 op x y)]
                       [(x . xs) (safe-apply-n op (cons x xs))])]))

(define (bitvector-type-error name . args)
  (arguments-error name "expected bitvectors of same length" "arguments" args))
 
(define (safe-apply-1 op x)
  (match x
    [(? is-bitvector?) (op x)]
    [(union xs _)
     (merge+ 
      (let loop ([xs xs])
        (match xs
          [(list) '()]
          [(list (cons gx (? is-bitvector? vx)) rest ...)
           (cons (cons gx (op vx)) (loop rest))]
          [(list _ rest ...) (loop rest)]))
      #:unless (length xs) 
      #:error (bitvector-type-error (object-name op) x))]
    [_ (assert #f (bitvector-type-error (object-name op) x))]))

(define (safe-apply-2 op x y)
  (assert (and (typed? x) (typed? y)) (bitvector-type-error (object-name op) x y))
  (match* (x y)
    [((app get-type (? bitvector? tx)) _) 
     (if (equal? tx (get-type y))
         (op x y) 
         (op x (type-cast tx y (object-name op))))]
    [(_ (app get-type (? bitvector? ty))) 
     (op (type-cast ty x (object-name op)) y)]
    [((union xs _) (union ys _))
     (merge+
      (let loop ([xs xs])
        (match xs
          [(list) '()]
          [(list (cons gx (and (? typed? vx) (app get-type (? bitvector? tx)))) rest ...)
           (match ys
             [(list _ ... (cons gy (and (? typed? vy) (app get-type (== tx)))) _ ...)
              (match (&& gx gy)
                [#f (loop rest)]
                [g  (cons (cons g (op vx vy)) (loop rest))])]
             [_ (loop rest)])]
          [(list _ rest ...)
           (loop rest)]))
      #:unless (max (length xs) (length ys))
      #:error (bitvector-type-error (object-name op) x y))]
    [(_ _) (assert #f (bitvector-type-error (object-name op) x y))]))

(define (safe-apply-n op xs)
  (assert (for/and ([x xs]) (typed? x)) (apply bitvector-type-error (object-name op) xs))
  (match xs
    [(list _ ... (app get-type (? bitvector? t)) _ ...)
     (apply op (for/list ([x xs]) 
                 (if (equal? (get-type x) t) x (type-cast t x (object-name op)))))]
    [(list (union vs _) (union ws _) ...)  
     (merge+
      (let loop ([vs vs])
        (match vs
          [(list) '()]
          [(list (cons gx (and (? typed? vx) (app get-type (? bitvector? tx)))) rest ...)
           (match ws
             [(list (list _ ... (cons gy (and (? typed? vy) (app get-type (== tx)))) _ ...) ...)
              (match (apply && gx gy)
                [#f (loop rest)]
                [g  (cons (cons g (apply op vx vy)) (loop rest))])]
             [_ (loop rest)])]
          [(list _ rest ...)
           (loop rest)]))
      #:unless (apply max (length vs) (map length ws))
      #:error (apply bitvector-type-error (object-name op) xs))]
    [_ (assert #f (apply bitvector-type-error (object-name op) xs))]))

(define-syntax-parameter finitize
  (syntax-rules () [(_ e t) e]))

(define-syntax-rule (define-lifted-operator @bvop bvop type)
  (define-operator @bvop
    #:identifier 'bvop
    #:range type
    #:unsafe bvop
    #:safe (lift-op bvop)))
    

;; ----------------- Bitvector Comparison Operators ----------------- ;; 

(define (bveq x y) 
  (match* (x y)
    [((bv u _) (bv v _)) (= u v)]
    [(_ (== x)) #t]
    [((expression (== ite) a (bv b _) (bv c _)) (bv d _))
     (|| (&& a (= b d)) (&& (! a) (= c d)))]
    [((bv d t) (expression (== ite) a (bv b _) (bv c _)))
     (|| (&& a (= b d)) (&& (! a) (= c d)))]
    [((expression (== ite) a (bv b t) (bv c _)) (expression (== ite) d (bv e _) (bv f _)))
     (let ([b=e (= b e)] 
           [b=f (= b f)] 
           [c=e (= c e)] 
           [c=f (= c f)])
       (or (and b=e b=f c=e c=f)
           (|| (&& a d b=e) (&& a (! d) b=f) (&& (! a) d c=e) (&& (! a) (! d) c=f))))]
    [(_ _) (sort/expression @bveq x y)]))

(define bvslt
  (bitwise-comparator (x y) < @bvslt
   [(_ (== x)) #f]
   [(_ (? bvsmax?)) (! (bveq x y))]
   [((? bvsmax?) _) #f]
   [(_ (? bvsmin?)) #f]
   [((? bvsmin?) _) (! (bveq x y))]))

(define bvsle
  (bitwise-comparator (x y) <= @bvsle
    [(_ (== x)) #t]
    [(_ (? bvsmax?)) #t]
    [((? bvsmax?) _) (bveq x y)]
    [(_ (? bvsmin?)) (bveq x y)]
    [((? bvsmin?) _) #t]))

(define-values (bvult bvule) 
  (syntax-parameterize 
   ([finitize (syntax-rules () [(_ e t) (ufinitize e (bitvector-size t))])])
   (values
    (bitwise-comparator (x y) < @bvult
     [(_ (== x)) #f]
     [(_ (bv -1 _)) (! (bveq x y))]
     [((bv -1 _) _) #f]
     [(_ (bv 0 _)) #f]
     [((bv 0 _) _) (! (bveq x y))])
    (bitwise-comparator (x y) <= @bvule
     [(_ (== x)) #t]
     [(_ (bv -1 _)) #t]
     [((bv -1 _) _) (bveq x y)]
     [(_ (bv 0 _)) (bveq x y)]
     [((bv 0 _) _) #t]))))

(define (bvsgt x y) (bvslt y x))
(define (bvsge x y) (bvsle y x))
(define (bvugt x y) (bvult y x))
(define (bvuge x y) (bvule y x))

(define-lifted-operator @bveq bveq T*->boolean?)
(define-lifted-operator @bvslt bvslt T*->boolean?)
(define-lifted-operator @bvsgt bvsgt T*->boolean?)
(define-lifted-operator @bvsle bvsle T*->boolean?)
(define-lifted-operator @bvsge bvsge T*->boolean?)
(define-lifted-operator @bvult bvult T*->boolean?)
(define-lifted-operator @bvugt bvugt T*->boolean?)
(define-lifted-operator @bvule bvule T*->boolean?)
(define-lifted-operator @bvuge bvuge T*->boolean?)

;; ----------------- Bitvector Bitwise Operators ----------------- ;;

(define bvnot (bitwise-negation bitwise-not bvnot @bvnot))
(define bvand (bitwise-connective bitwise-and bvand @bvand @bvor -1 0))
(define bvor (bitwise-connective bitwise-ior bvor @bvor @bvand 0 -1))
(define bvxor (bitwise-adder bitwise-xor bvxor @bvxor simplify-bvxor))

(define/match (max-shift? b)
  [((bv a (bitvector size))) (>= (ufinitize a size) size)]
  [(_) #f])

(define (bvshl x y)
  (match* (x y)
    [((bv a (and (bitvector size) t)) (bv b _))
     (bv (sfinitize (arithmetic-shift a (min (ufinitize b size) size)) size) t)]
    [(_ (bv 0 _)) x]
    [((bv 0 _) _) x]
    [(_ (? max-shift?)) (bv 0 (get-type x))]
    [(_ _) (expression @bvshl x y)]))

(define (bvlshr x y)
  (match* (x y)
    [((bv a (and (bitvector size) t)) (bv b _))
     (bv (sfinitize (arithmetic-shift (ufinitize a size) (- (min (ufinitize b size) size))) size) t)]
    [(_ (bv 0 _)) x]
    [((bv 0 _) _) x]
    [(_ (? max-shift?)) (bv 0 (get-type x))]
    [(_ _) (expression @bvlshr x y)]))

(define (bvashr x y)
  (match* (x y)
    [((bv a (and (bitvector size) t)) (bv b _))
     (bv (sfinitize (arithmetic-shift a (- (min (ufinitize b size) size))) size) t)]
    [(_ (bv 0 _)) x]
    [((bv 0 _) _) x]
    [((bv -1 _) _) x]
    [((app get-type t) (? max-shift?)) 
     (ite (bveq (bv 0 t) (bvand x (bv (bvsmin t) t))) (bv 0 t) (bv -1 t))]
    [(_ _) (expression @bvashr x y)]))

(define-lifted-operator @bvnot bvnot T*->T)
(define-lifted-operator @bvand bvand T*->T)
(define-lifted-operator @bvor bvor T*->T)
(define-lifted-operator @bvxor bvxor T*->T)
(define-lifted-operator @bvshl bvshl T*->T)
(define-lifted-operator @bvlshr bvlshr T*->T)
(define-lifted-operator @bvashr bvashr T*->T)

;; ----------------- Simplification ruules for bitwise operators ----------------- ;;

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

(define bvmul
  (case-lambda 
    [(x) x]
    [(x y) (or
            (simplify-bvmul x y)
            (sort/expression @bvmul x y))]
    [(x . xs)
     (let*-values ([(lits terms) (partition bv? (cons x xs))]
                   [(t) (get-type x)]
                   [(lit) (sfinitize
                           (for/fold ([out 1]) ([lit lits])
                            (* out (bv-value lit)))
                           (bitvector-size t))])
       (if (or (= lit 0) (null? terms)) 
           (bv lit t)
           (match (simplify* (if (null? lits)
                                 terms 
                                 (cons (bv lit t) terms)) 
                             simplify-bvmul)
             [(list y) y]
             [(list a ... (? bv? b) c ...) 
                 (apply expression @bvmul b (sort (append a c) term<?))]
             [ys (apply expression @bvmul (sort ys term<?))])))]))

(define (bvudiv x y)
  (match* (x y)
    [(_ (bv 0 t)) (bv -1 t)]
    [(_ (bv 1 _)) x]
    [((bv a (and t (bitvector size))) (bv b _)) 
     (bv (sfinitize (quotient (ufinitize a size) (ufinitize b size)) size) t)]
    [(_ (bv -1 t)) (ite (bveq x y) (bv 1 t) (bv 0 t))]
    [((bv 0 t) _) (ite (bveq x y) (bv -1 t) x)]
    [((app get-type t) (== x)) (ite (bveq y (bv 0 t)) (bv -1 t) (bv 1 t))]
    [((expression (== ite) c (? bv? a) (? bv? b)) (? bv? d))
     (ite c (bvudiv a d) (bvudiv b d))]
    [((? bv? d) (expression (== ite) c (? bv? a) (? bv? b)))
     (ite c (bvudiv d a) (bvudiv d b))]
    [(_ _) (expression @bvudiv x y)]))

(define (bvsdiv x y)
  (match* (x y)
    [(_ (bv 1 _)) x]
    [((bv a (and t (bitvector size))) (bv b _))
     (if (= b 0) 
         (if (< a 0) (bv 1 t) (bv -1 t))
         (bv (sfinitize (quotient a b) size) t))]
    [(_ (bv 0 t)) (ite (bvslt x y) (bv 1 t) (bv -1 t))]
    [(_ (bv -1 t)) (bvneg x)]
    [(_ (and (bv _ t) (? bvsmin?))) (ite (bveq x y) (bv 1 t) (bv 0 t))]
    [((bv 0 t) _) (ite (bveq x y) (bv -1 t) x)]
    [((app get-type t) (== x)) (ite (bveq y (bv 0 t)) (bv -1 t) (bv 1 t))]
    [((app get-type t) (expression (== @bvneg) (== x))) (ite (bveq x (bv (bvsmin t) t)) (bv 1 t) (bv -1 t))]
    [((expression (== @bvneg) (== y)) (app get-type t)) (ite (bveq y (bv (bvsmin t) t)) (bv 1 t) (bv -1 t))]
    [((expression (== ite) c (? bv? a) (? bv? b)) (? bv? d))
     (ite c (bvsdiv a d) (bvsdiv b d))]
    [((? bv? d) (expression (== ite) c (? bv? a) (? bv? b)))
     (ite c (bvsdiv d a) (bvsdiv d b))]
    [(_ _) (expression @bvsdiv x y)]))

(define (bvurem x y)
  (match* (x y)
    [(_ (bv 0 _)) x]
    [((bv 0 t) _) x]
    [(_ (bv 1 t)) (bv 0 t)]
    [((bv a (and t (bitvector size))) (bv b _)) 
     (bv (sfinitize (remainder (ufinitize a size) (ufinitize b size)) size) t)]
    [(_ (bv -1 t)) (ite (bveq x y) (bv 0 t) x)]
    [((app get-type t) (== x)) (bv 0 t)]
    [((expression (== ite) c (? bv? a) (? bv? b)) (? bv? d))
     (ite c (bvurem a d) (bvurem b d))]
    [((? bv? d) (expression (== ite) c (? bv? a) (? bv? b)))
     (ite c (bvurem d a) (bvurem d b))]
    [(_ _) (expression @bvurem x y)]))

(define bvsrem 
  (bitwise-signed-remainder (x y) remainder bvsrem @bvsrem 
    [(_ (and (bv _ t) (? bvsmin?))) (ite (bveq x y) (bv 0 t) x)]))

(define bvsmod (bitwise-signed-remainder (x y) modulo bvsmod @bvsmod)) 
                            
(define-lifted-operator @bvneg bvneg T*->T)
(define-lifted-operator @bvadd bvadd T*->T)
(define-lifted-operator @bvsub bvsub T*->T)
(define-lifted-operator @bvmul bvmul T*->T)
(define-lifted-operator @bvudiv bvudiv T*->T)
(define-lifted-operator @bvsdiv bvsdiv T*->T)
(define-lifted-operator @bvurem bvurem T*->T)
(define-lifted-operator @bvsrem bvsrem T*->T)
(define-lifted-operator @bvsmod bvsmod T*->T)

;; ----------------- Simplification rules for arithmetic operators ----------------- ;;

; Simplification rules for bvadd.
(define (simplify-bvadd x y)
  (match* (x y)
    [((bv a t) (bv b _)) (bv (sfinitize (+ a b) (bitvector-size t)) t)]
    [((bv 0 _) _) y]
    [(_ (bv 0 _)) x]
    [((? expression?) (? expression?)) (or (simplify-bvadd:expr/term x y)
                                           (simplify-bvadd:expr/term y x))]
    [((? expression?) _) (simplify-bvadd:expr/term x y)]
    [(_ (? expression?)) (simplify-bvadd:expr/term y x)]
    [(_ _) #f]))
                
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
    [((expression (== ite) a (? bv? b) (? bv? c)) (? bv?)) (ite a (bvadd b y) (bvadd c y))]
    [((expression (== @bvadd) a ...) (expression (== @bvadd) b ...))
     (let ([alen (length a)] 
           [blen (length b)])
       (and (<= alen blen) (<= (- blen alen) 1)
            (let* ([-a (map bvneg a)]
                   [-a (if (bv? (car -a)) 
                           (cons (car -a) (sort (cdr -a) term<?))
                           (sort -a term<?))])
              (and (sublist? -a b)
                   (if (= alen blen) 
                       (bv 0 (get-type x))
                       (car (remove* -a b)))))))]
    [((expression (== @bvmul) (? bv? a) b) (expression (== @bvmul) (? bv? c) b))
     (bvmul (bvadd a c) b)]
    [((expression (== @bvmul) a b) (expression (== @bvmul) c d))
     (let-values ([(u v w) (cond [(equal? a c) (values a b d)]
                                 [(equal? a d) (values a b c)]
                                 [(equal? b c) (values b a d)]
                                 [(equal? b d) (values b a c)]
                                 [else (values #f #f #f)])])
       (and u 
            (match (simplify-bvadd v w)
              [#f #f]
              [z (bvmul z u)])))]
    [(_ _) #f]))

; Simplification rules for bvmul.
(define (simplify-bvmul x y)
  (match* (x y)
    [((bv a t) (bv b _)) (bv (sfinitize (* a b) (bitvector-size t)) t)]
    [((bv 0 _) _) x]
    [((bv 1 _) _) y]
    [((bv -1 _) _) (bvneg y)]
    [(_ (bv 0 _)) y]
    [(_ (bv 1 _)) x]
    [(_ (bv -1 _)) (bvneg x)]
    [((expression (== @bvmul) (? bv? a) b) (? bv? c))
     (bvmul (bvmul a c) b)]
    [((? bv? c) (expression (== @bvmul) (? bv? a) b))
     (bvmul (bvmul a c) b)]
    [(_ _) #f]))

;; ----------------- Concatenation and Extraction ----------------- ;;

(define (bvcoerce x [caller 'bvcoerce])
  (assert (typed? x) (type-error caller 'bitvector? x))
  (match x
    [(app get-type (? bitvector?)) x]
    [(union xs) (merge+ (for/list ([gx xs] #:when (is-bitvector? (cdr gx))) gx)
                        #:unless (length xs)
                        #:error (type-error caller 'bitvector? x))]
    [_ (assert #f (type-error caller 'bitvector? x))]))

(define concat
  (case-lambda
    [(x) x]
    [(x y)
     (match* (x y)
       [((bv a (bitvector size-a)) (bv b (bitvector size-b)))
        (bv (bitwise-ior (arithmetic-shift a size-b) (ufinitize b size-b)) (bitvector-type (+ size-a size-b)))]
       [((expression (== @extract) i j e) (expression (== @extract) k n e))
        (if (= j (add1 k)) (extract i n e) (expression @concat x y))]
       [(_ _) (expression @concat x y)])]
    [(x . ys) (for/fold ([out x]) ([y ys]) (concat out y))]))

(define-operator @concat
  #:identifier 'concat
  #:range (lambda xs (bitvector-type (for/sum ([x xs]) (bitvector-size (get-type x)))))
  #:unsafe concat 
  #:safe (case-lambda
           [(x) (bvcoerce x 'concat)]
           [(x y)
            (match* ((bvcoerce x 'concat) (bvcoerce y 'concat))
              [((union xs) (union ys))
               (merge+ (for*/list ([gx xs] [gy ys] [g (in-value (&& (car gx) (car gy)))] #:when g)     
                         (cons g (concat (cdr gx) (cdr gy))))
                       #:unless (* (length xs) (length ys))
                       #:error (arguments-error 'concat "infeasible arguments" "x" x "y" y))]
              [((union xs) y) (merge** xs (concat _ y))]
              [(x (union ys)) (merge** ys (concat x _))]
              [(x y) (concat x y)])]
           [(x . ys) (for/fold ([out x]) ([y ys]) (@concat out y))]))

; i and j must be concrete integers with bw > i >= j > 0, where bw is the bitwidth of x
(define (extract i j x) 
  (define len (add1 (- i j)))
  (match* (i j x)
    [((== (sub1 (bitvector-size (get-type x)))) 0 _) x]
    [(_ _ (bv b _)) 
     (bv (sfinitize (bitwise-and (bitwise-not (arithmetic-shift -1 len)) (arithmetic-shift b (- j))) len) 
         (bitvector-type len))]
    [(_ 0 (expression (== @concat) _ (and (? typed? (app get-type (bitvector (== len)))) a))) a]
    [(_ _ (expression (== @concat) 
                      (and (? typed? (app get-type (bitvector (== len)))) a) 
                      (? typed? (app get-type (bitvector (== j)))))) a]
    [(_ _ _) (expression @extract i j x)]))
        
(define-operator @extract
  #:identifier 'extract
  #:range (lambda (i j x) (bitvector-type (add1 (- i j))))
  #:unsafe extract
  #:safe 
  (local [(define-syntax-rule (extract*-err x i j) 
            (arguments-error 'extract "expected (size-of x) > i >= j >= 0" "x" x "i" i "j" j))
          (define (extract* i j x)
            (define size (bitvector-size (get-type x)))
            (assert (@> size i) (arguments-error 'extract "expected (size-of x) > i" "x" x "i" i))
            (match* (i j)
              [((? number?) (? number?)) (extract i j x)]
              [(_ (? number?)) (merge+ (for/list ([n (in-range j size)])
                                         (cons (@= n i) (extract n j x)))
                                       #:unless (- size j) #:error (extract*-err x i j))]
              [((? number?) _) (merge+ (for*/list ([k (in-range i -1 -1)])
                                               (cons (@= k j) (extract i k x)))
                                       #:unless (+ i 1) #:error (extract*-err x i j))]
              [(_ _) (merge+ (for*/list ([n size] [k (add1 n)])
                               (cons (&& (@= n i) (@= k j)) (extract n k x)))
                             #:unless (+ size (/ (* size (- size 1)) 2)) #:error (extract*-err x i j))]))]
    (lambda (@i @j @x)
      (define i (type-cast @integer? @i 'extract))
      (define j (type-cast @integer? @j 'extract))
      (define x (bvcoerce @x 'extract))
      (assert (or (integer? i) (term? i)) (arguments-error 'extract "expected an integer i" "i" i))
      (assert (or (integer? j) (term? j)) (arguments-error 'extract "expected an integer j" "j" j))
      (assert (@>= i j) (arguments-error 'extract "expected i >= j" "i" i "j" j))
      (assert (@>= j 0) (arguments-error 'extract "expected j >= 0" "j" j))
      (match x 
        [(? union?) (for/all ([y x]) (extract* i j y))]
        [_ (extract* i j x)]))))
     

;; ----------------- Extension and Coercion ----------------- ;;

; Assumes that (bitvector-size t) >= (bitvector-size (get-type v))
(define (extend v t finitize @bvop)
  (match* (v t)
    [((app get-type (== t)) _) v]
    [((bv a (bitvector s)) _) (bv (finitize a s) t)]
    [((expression (== @bvop) x _) _) (expression @bvop x t)]
    [(_ _) (expression @bvop v t)]))

(define-syntax-rule (@extend-err v t)
   (arguments-error 'extend "expected (bitvector-size t) >= (bitvector-size (get-type v))" "v" v "t" t))

(define-syntax-rule (@extend extend)
  (lambda (@v @t)
    (match* ((bvcoerce @v 'extend) @t)
      [((union vs) (union ts))
       (merge+ (for*/list ([gt ts] #:when (bitvector? (cdr gt))
                           [gv vs] #:when (<= (bitvector-size (get-type (cdr gv))) (bitvector-size (cdr gt))))
                 (cons (&& (car gt) (car gv)) (extend (cdr gv) (cdr gt))))
               #:unless (* (length vs) (length ts)) #:error (@extend-err @v @t))]
      [((union vs) (bitvector st))
       (merge+ (for/list ([gv vs] #:when (<= (bitvector-size (get-type (cdr gv))) st))
                 (cons (car gv) (extend (cdr gv) @t)))
               #:unless (length vs) #:error (@extend-err @v @t))]
      [((and (app get-type (bitvector sv)) v) (union ts))
       (merge+ (for/list ([gt ts] #:when (and (bitvector? (cdr gt)) (<= sv (bitvector-size (cdr gt)))))
                 (cons (car gt) (extend v (cdr gt))))
               #:unless (length ts) #:error (@extend-err @v @t))]
      [((and (app get-type (bitvector sv)) v) (bitvector st))
       (assert (<= sv st) (@extend-err @v @t))
       (extend v @t)]
      [(_ _) (assert #f (@extend-err @v @t))])))

(define (coercion-type v t) t)

(define (sign-extend v t) (extend v t sfinitize @sign-extend))
(define (zero-extend v t) (extend v t ufinitize @zero-extend))

(define-operator @sign-extend
  #:identifier 'sign-extend
  #:range coercion-type
  #:unsafe sign-extend
  #:safe (@extend sign-extend))
       
(define-operator @zero-extend
  #:identifier 'zero-extend
  #:range coercion-type
  #:unsafe zero-extend
  #:safe (@extend zero-extend))

(define (integer->bitvector v t)
  (match v
    [(? integer?) (@bv v t)]
    ; This optimization is valid only when integer bitwidth >= (bitvector-size t).
    ;[(expression (== @bitvector->integer) (and (app get-type (== t)) x)) x]
    [_ (expression @integer->bitvector v t)]))

(define (bitvector->integer v)
  (match v
    [(bv a _) a]
    [_ (expression @bitvector->integer v)]))

(define (bitvector->natural v)
  (match v
    [(bv a (bitvector sz)) (ufinitize a sz)]
    [_ (expression @bitvector->natural v)]))

(define-syntax-rule (@bv->* bvop)
  (lambda (@v)
    (match (bvcoerce @v 'bvop)
      [(union vs) (merge** vs bvop)]
      [v (bvop v)])))

(define-operator @integer->bitvector
  #:identifier 'integer->bitvector
  #:range coercion-type
  #:unsafe integer->bitvector
  #:safe 
  (lambda (@v @t)
    (match* ((type-cast @integer? @v 'integer->bitvector) @t)
      [(v (union ts))
       (merge+ (for/list ([gt ts] #:when (bitvector? (cdr gt)))
                 (cons (car gt) (integer->bitvector v (cdr gt))))
               #:unless (length ts) #:error (arguments-error 'integer->bitvector "expected a bitvector type t" "t" @t))]
      [(v (? bitvector? t)) (integer->bitvector v t)]
      [(_ _) (assert #f (arguments-error 'integer->bitvector "expected a bitvector type t" "t" @t))])))

(define-operator @bitvector->integer
  #:identifier 'bitvector->integer
  #:range T*->integer?
  #:unsafe bitvector->integer
  #:safe (@bv->* bitvector->integer))
             
(define-operator @bitvector->natural
  #:identifier 'bitvector->natural
  #:range T*->integer?
  #:unsafe bitvector->natural
  #:safe (@bv->* bitvector->natural))

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
    [(x . xs) 
     (let*-values ([(lits terms) (partition bv? (cons x xs))]
                   [(lit) (for/fold ([out iden]) ([lit lits])
                            (op out (bv-value lit)))]
                   [(t) (get-type x)])
       (if (or (= lit !iden) (null? terms)) 
           (bv lit t)
           (match (simplify-connective* @bvop @bvco (bv !iden t) (remove-duplicates terms))
             [(list (bv u _)) (bv (op lit u) t)]
             [(list y) (bvop (bv lit t) y)]
             [ys (if (= lit iden)
                     (apply expression @bvop (sort ys term<?))
                     (apply expression @bvop (bv lit t) (sort ys term<?)))])))]))

; Simplification rules for bitwise and/or. Assumes that 
; neither x nor y are iden or !iden.
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

; Simplification rules for bitwise and/or, applied to fix point. 
; Assumes that the xs list contains no literals, only terms.
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

; Partial evaluation rules for adders (bvxor and bvadd).
(define-syntax-rule (bitwise-adder op bvop @bvop simplify-bvop)
  (case-lambda 
    [(x) x]
    [(x y) (or (simplify-bvop x y)
               (sort/expression @bvop x y))]
    [(x . xs)
     (let*-values ([(lits terms) (partition bv? (cons x xs))]
                   [(lit) (for/fold ([out 0]) ([lit lits]) (op out (bv-value lit)))]
                   [(t) (get-type x)])
       (if (null? terms)
           (bv (finitize lit t) t)
           (match (simplify* (if (null? lits) 
                                 terms 
                                 (cons (bv (finitize lit t) t) terms)) 
                             simplify-bvop)
             [(list y) y]
             [(list a (... ...) (? bv? b) c (... ...)) 
              (apply expression @bvop b (sort (append a c) term<?))]
             [ys (apply expression @bvop (sort ys term<?))])))]))

; Partial evaluation rules for comparators (bvslt, bvsle, bvult, bule).
(define-syntax-rule (bitwise-comparator (x y) op @bvop expr ...)
  (lambda (x y)
    (match* (x y)
      [((bv a t) (bv b _)) (op (finitize a t) (finitize b t))]
      expr ...
      [((expression (== ite) a (bv b t) (bv c _)) (bv d _))
       (|| (&& a (op (finitize b t) (finitize d t))) 
           (&& (! a) (op (finitize c t) (finitize d t))))]
      [((bv d t) (expression (== ite) a (bv b _) (bv c _)))
       (|| (&& a (op (finitize d t) (finitize b t))) 
           (&& (! a) (op (finitize d t) (finitize c t))))]
      [((expression (== ite) a (bv b t) (bv c _)) (expression (== ite) d (bv e _) (bv f _)))
       (let ([b<e (op (finitize b t) (finitize e t))] 
             [b<f (op (finitize b t) (finitize f t))] 
             [c<e (op (finitize c t) (finitize e t))] 
             [c<f (op (finitize c t) (finitize f t))])
         (or (and b<e b<f c<e c<f)
             (|| (&& a d b<e) (&& a (! d) b<f) (&& (! a) d c<e) (&& (! a) (! d) c<f))))]
      [(_ _) (expression @bvop x y)])))

; Partial evaluation rules for signed remainder / modulo (bvsrem, bvsmod).
(define-syntax-rule (bitwise-signed-remainder (x y) op bvop @bvop expr ...)
  (lambda (x y)
    (match* (x y)
      [(_ (bv 1 t)) (bv 0 t)]
      [(_ (bv -1 t)) (bv 0 t)]
      [(_ (bv 0 t)) x]
      [((bv 0 t) _) x]
      [((bv a (and t (bitvector size))) (bv b _)) (bv (sfinitize (op a b) size) t)]
      expr ...
      [((app get-type t) (== x)) (bv 0 t)]
      [((app get-type t) (expression (== @bvneg) (== x))) (bv 0 t)]
      [((expression (== @bvneg) (== y)) (app get-type t)) (bv 0 t)]
      [((expression (== ite) c (? bv? a) (? bv? b)) (? bv? d))
       (ite c (bvop a d) (bvop b d))]
      [((? bv? d) (expression (== ite) c (? bv? a) (? bv? b)))
       (ite c (bvop d a) (bvop d b))]
      [(_ _) (expression @bvop x y)])))




