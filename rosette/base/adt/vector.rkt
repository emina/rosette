#lang racket

(require (for-syntax racket/syntax "../core/lift.rkt") 
         racket/provide 
         (only-in racket/unsafe/ops [unsafe-car car] [unsafe-cdr cdr])
         "../core/safe.rkt" "../core/lift.rkt" "seq.rkt" "../core/forall.rkt" "generic.rkt"
         (only-in "list.rkt" @list?)
         (only-in "../form/control.rkt" @when)
         (only-in "../core/store.rkt" store!)
         (only-in "../core/term.rkt" define-lifted-type @any/c type-cast)
         (only-in "../core/equality.rkt" @eq? @equal?)
         (only-in "../core/bool.rkt" instance-of? && ||)
         (only-in "../core/real.rkt" @integer? @= @<= @< @- @+)
         (only-in "../core/union.rkt" union)
         (only-in "../core/merge.rkt" merge))

(provide (filtered-out with@ (all-defined-out))
         (rename-out [vector @vector] [vector-immutable @vector-immutable]))
                  
(define-lifted-type @vector?  
  #:base vector?
  #:is-a? (instance-of? vector? @vector?)      
  #:methods
  [(define (type-eq? self xs ys)
     (or (eq? xs ys)
         (and (immutable? xs) (immutable? ys) (vector=? @eq? xs ys))))
   (define (type-equal? self xs ys) (vector=? @equal? xs ys))
   (define (type-cast self v [caller 'type-cast])
     (adt-type-cast v #:type vector? #:lifted @vector? #:caller caller))  
   (define (type-compress self force? ps)
     (let-values ([(immutable mutable) (partition (compose1 immutable? cdr) ps)])
       (append (for/list ([p (unsafe/compress immutable)])
                 (cons (car p) (vector->immutable-vector (cdr p))))
               (if force? (unsafe/compress mutable) mutable))))
   (define (type-construct self vals) (list->vector vals))
   (define (type-deconstruct self val) (vector->list val))])

(define (vector=? =? xs ys)
  (let ([len (vector-length xs)])
    (and (= len (vector-length ys))
         (let loop ([i 0] [eqs '()])
           (if (= i len) 
               (apply && eqs)
               (let ([eq (=? (vector-ref xs i) (vector-ref ys i))])
                 (and eq (loop (add1 i) (cons eq eqs)))))))))

(define (unsafe/compress ps)
  (seq-compress ps vector-length vector-map : 
                [(for/seq ([x vec] rest ...) body)
                 (for/vector #:length (vector-length vec)
                   ([x vec] rest ...) body)]))

(define/lift (vector-length vector->list vector->immutable-vector) :: vector? -> @vector?)
(define/lift (list->vector) :: list? -> @list?)

(define/lift/ref vector-ref : (vector? vector-length) -> @vector?)
(define/lift/append vector-append : (vector? vector) -> @vector?)

(define (merge-set! vec idx val guard)
  (for ([i (in-range (vector-length vec))])
    (store! vec i (merge (&& guard (@= i idx)) val (vector-ref vec i)) vector-ref vector-set!)))

(define (@vector-set! vec idx val)
  ;(printf "vector-set! ~a ~a ~a\n" (eq-hash-code vec) idx val)
  (if (and (vector? vec) (number? idx))
      (store! vec idx val vector-ref vector-set!)
      (match* ((type-cast @vector? vec 'vector-set!) (type-cast @integer? idx 'vector-set!))
        [((? vector? vs) (? number? idx))
         (store! vs idx val vector-ref vector-set!)]
        [((? vector? vs) idx)
         (assert-bound [0 @<= idx @< (vector-length vs)] 'vector-set!)
         (merge-set! vs idx val #t)]
        [((union vs) (? number? idx))
         (assert-bound [0 <= idx] 'vector-set!)
         (assert-|| (for/list ([v vs] #:when (< idx (vector-length (cdr v))))
                      (let ([guard (car v)]
                            [vec (cdr v)])
                        (store! vec idx (merge guard val (vector-ref vec idx)) vector-ref vector-set!)
                        guard))
                    #:unless (length vs)
                    (index-too-large-error 'vector-set! vec idx))]
        [((union vs) idx)
         (assert-bound [0 @<= idx @< (merge** vs vector-length)] 'vector-set!)
         (for ([v vs])
           (merge-set! (cdr v) idx val (car v)))])))

(define (@vector-fill! vec val)
  (match (type-cast @vector? vec 'vector-fill!)
    [(? vector? vs)
     (for ([i (in-range (vector-length vs))])
       (store! vs i val vector-ref vector-set!))]
    [(union vs)
     (for ([v vs])
       (let ([guard (car v)]
             [vec (cdr v)])
         (for ([i (in-range (vector-length vec))])
           (store! vec i (merge guard val (vector-ref vec i)) vector-ref vector-set!))))]))

; Vector copy helper procedure.  Requires dest and src to be 
; vectors (rather than unions of vectors), and dest-start, src-start
; and len to be concrete in-range numbers.
(define (concrete/vector-copy! dest dest-start src src-start len)
  (for ([idx len])
    (@vector-set! dest (+ dest-start idx) (vector-ref src (+ src-start idx)))))

(define-syntax-rule (in-concrete nat max-nat)
  (if (number? nat) (in-value nat) (in-range max-nat)))

(define @vector-copy! 
  (case-lambda
    [(dest dest-start src) 
     (@vector-copy! dest dest-start src 0)]
    [(dest dest-start src src-start)
     (let ([dest (type-cast @vector? dest 'vector-copy!)]
           [dest-start (type-cast @integer? dest-start 'vector-copy!)]
           [src (type-cast @vector? src 'vector-copy!)]
           [src-start (type-cast @integer? src-start 'vector-copy!)])
       (for*/all ([d dest] [s src])
         (@vector-copy! d dest-start s src-start (vector-length s))))]
    [(dest dest-start src src-start src-end)
     (let ([dest (type-cast @vector? dest 'vector-copy!)]
           [dest-start (type-cast @integer? dest-start 'vector-copy!)]
           [src (type-cast @vector? src 'vector-copy!)]
           [src-start (type-cast @integer? src-start 'vector-copy!)]
           [src-end (type-cast @integer? src-end 'vector-copy!)])
       (assert-bound [0 @<= dest-start] 'vector-copy)
       (assert-bound [0 @<= src-start @<= src-end] 'vector-copy!)
       (define len (@- src-end src-start))
       (define dest-end (@+ dest-start len))
       (for*/all ([d dest] [s src])
         (let ([dest-len (vector-length d)]
               [src-len (vector-length s)])
         (assert-bound [dest-end @<= dest-len] 'vector-copy!)
         (assert-bound [src-end @<= src-len] 'vector-copy!)
         (cond 
           [(equal? 0 len) (void)]
           [(and (number? dest-start) (number? src-start) (number? len))
            (concrete/vector-copy! d dest-start s src-start len)]
           [(number? len)
            (for* ([concrete-dest-start (in-concrete dest-start (add1 (- dest-len len)))]
                   [concrete-src-start  (in-concrete src-start  (add1 (- src-len len)))])
              (@when (&& (@= dest-start concrete-dest-start)
                            (@= src-start concrete-src-start))
                (concrete/vector-copy! d concrete-dest-start s concrete-src-start len)))]
           [else
            (for* ([concrete-len (add1 (min (if (number? dest-start) (- dest-len dest-start) dest-len)
                                            (if (number? src-start)  (- src-len src-start) src-len)))]
                   [concrete-dest-start (in-concrete dest-start (add1 (- dest-len concrete-len)))]
                   [concrete-src-start  (in-concrete src-start  (add1 (- src-len concrete-len)))])
              (@when (&& (@= len concrete-len)
                            (@= dest-start concrete-dest-start)
                            (@= src-start concrete-src-start))
                (concrete/vector-copy! d concrete-dest-start s concrete-src-start concrete-len)))]))))]))
            
            
           
            
           
         
         
         
           
    
    
    
       
       
     
