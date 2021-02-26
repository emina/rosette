#lang racket

(require
  (only-in "list.rkt" @list?)
  (only-in "vector.rkt" @vector? @vector-set!)
  (only-in "../core/lift.rkt" lift-id)
  (only-in "../core/forall.rkt" for/all for*/all)
  (only-in "../core/term.rkt" get-type type-cast term?)
   "../core/union.rkt"
  "../core/bitvector.rkt"
  "../core/merge.rkt"
  "../core/safe.rkt")

(provide @list-ref-bv @list-set-bv
         @take-bv @take-right-bv
         @drop-bv @drop-right-bv
         @split-at-bv @split-at-right-bv
         @length-bv
         @vector-ref-bv @vector-set!-bv @vector-length-bv)

(define (bv-lit-or-term? v)
  (or (bv? v) (and (term? v) (bitvector? (get-type v)))))

(define-syntax-rule (lift-body #:with (id xs idx seq-length) #:type t #:max n #:body body ...)
  (let* ([t   (get-type idx)]
         [2^k (expt 2 (bitvector-size t))]
         [sz  (seq-length xs)]
         [n   (min sz 2^k)])
    (when (>= (- 2^k 1) sz)
      (assert (@bvult idx  (@integer->bitvector sz t))
              (index-too-large-error 'id xs idx)))
    body ...))

(define-syntax (define-lift-bv stx)
  (syntax-case stx ()
    [(_ (proc-bv xs idx arg ...) @seq? seq?)
     #`(define-lift-bv #,(lift-id #'proc-bv) (proc-bv xs idx arg ...) @seq? seq?)]
    [(_ @proc-bv (proc-bv xs idx arg ...) @seq? seq?)
     #'(define (@proc-bv xs idx arg ...)
         (if (and (seq? xs) (bv-lit-or-term? idx))
             (proc-bv xs idx arg ...)
             (match* ((type-cast @seq? xs 'proc-bv)
                      (bvcoerce idx 'proc-bv))
               [((? seq? xs) (? bv-lit-or-term? idx))
                (proc-bv xs idx arg ...)]
               [(xs idx)
                (for*/all ([xs xs][idx idx])
                  (proc-bv xs idx arg ...))])))]))

(define-syntax (define-length-bv stx)
  (syntax-case stx ()
    [(_ length-bv @seq? seq?  seq-length)
     #`(begin
         (define (length-bv xs t) ; (-> seq bitvector? @bv?)
           (@integer->bitvector (seq-length xs) t))
         (define (#,(lift-id #'length-bv) xs t)
           (match (type-cast @seq? xs 'length-bv)
             [(? seq? xs) (length-bv xs t)]
             [xs (for/all ([xs xs]) (length-bv xs t))])))]))

(define-syntax-rule (define-ref-bv ref-bv @seq? seq? seq-ref seq-length)
  (begin
    (define (ref-bv xs idx) ; (-> type? bv-lit-or-term? any/c)
      (if (bv? idx)
          (seq-ref xs (@bitvector->natural idx))
          (lift-body
           #:with (ref-bv xs idx seq-length)
           #:type t
           #:max  n
           #:body 
            (apply
             merge*
             (for/list ([x xs] [i n])
               (cons (@bveq (bv i t) idx) x))))))

    (define-lift-bv (ref-bv xs idx) @seq? seq?)))

; ---- list bv procedures ---- ;

(define-length-bv length-bv @list? list? length)
(define-ref-bv list-ref-bv @list? list? list-ref length)

(define (list-set-bv xs idx v)
  (if (bv? idx)
      (list-set xs (@bitvector->natural idx) v)
      (lift-body
       #:with (list-set-bv xs idx length)
       #:type t
       #:max  n
       #:body (for/list ([(x i) (in-indexed xs)])
                (if (< i n)
                    (merge (@bveq (bv i t) idx) v x)
                    x)))))

(define-lift-bv (list-set-bv xs idx v) @list? list?)

(define-syntax-rule (define-get-bv get-bv @seq? seq? seq-get seq-length)
  (begin
    (define (get-bv xs idx) ; (-> type? bv-lit-or-term? any/c)
      (if (bv? idx)
          (seq-get xs (@bitvector->natural idx))
          (let* ([t   (get-type idx)]
                 [2^k (expt 2 (bitvector-size t))]
                 [sz  (seq-length xs)]
                 [n   (add1 (min sz 2^k))])
            (when (> (- 2^k 1) sz)
              (assert (@bvule idx  (@integer->bitvector sz t))
                      (argument-error 'id "a number less than or equal to the list size" idx)))
            (apply
             merge*
             (for/list ([i n])
               (cons (@bveq (bv i t) idx)
                     (seq-get xs i)))))))
    (define-lift-bv (get-bv xs idx) @seq? seq?)))
    
(define-get-bv take-bv @list? list? take length)      
(define-get-bv take-right-bv @list? list? take-right length)
(define-get-bv drop-bv @list? list? drop length)      
(define-get-bv drop-right-bv @list? list? drop-right length)

(define (@split-at-bv xs idx)
  (values (@take-bv xs idx) (@drop-bv xs idx)))

(define (@split-at-right-bv xs idx)
  (values (@drop-right-bv xs idx) (@take-right-bv xs idx)))

; ---- vector bv procedures ---- ;

(define (vector-set!-bv xs idx v)
  (if (bv? idx)
      (@vector-set! xs (@bitvector->natural idx) v)
      (lift-body
       #:with (vector-set!-bv xs idx vector-length)
       #:type t
       #:max  n
       #:body 
       (for ([x xs] [i n])
         (@vector-set! xs i (merge (@bveq (bv i t) idx) v x))))))

(define-length-bv vector-length-bv @vector? vector? vector-length)
(define-ref-bv vector-ref-bv @vector? vector? vector-ref vector-length)
(define-lift-bv (vector-set!-bv xs idx v) @vector? vector?)
