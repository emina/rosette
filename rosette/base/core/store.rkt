#lang racket

(require "result.rkt" "merge.rkt")

(provide with-store store! merge-stores!
         location? location-base location-offset
         location-ref location-set!)

; The current-store parameter contains a store that 
; maps (abstract) memory locations to values.  Each mapped
; location identifies a storage cell that has been the mutated
; via a store! call in the dynamic extent of a call to
; with-store. The store maps each such location to the value
; that was stored at that location before the current call 
; to with-store.
(define current-store (make-parameter #f))

; A store maps abstract memory locations to their initial values.
; An abstract memory location identifies a storage cell that holds
; a single value; locations consist of a base object (e.g., a vector)
; and an offset value (e.g., the index 0) that identifies a
; storage cell within that object.
;
; A store uses a refs set, as returned by make-refs, to keep track
; of the locations that have been mutated via store! calls.
; The initial value of each mutated location is held in the store's
; vals list. This list maps locations to the values they held prior
; to the current call to with-store.
(struct store (refs [vals #:mutable]) #:transparent)

; Returns an empty store.
(define (make-store) (store (make-refs) (list)))

; Returns an empty set of base/offset pairs.
(define (make-refs) (make-hasheq))

; Adds the given base/offset pair to rs if not
; already present. Returns #t if rs changed as
; a result of this operation; otherwise returns #f.
(define (refs-add! rs base offset)
  (define bits (hash-ref rs base 0))
  (and (not (bitwise-bit-set? bits offset))
       (hash-set! rs base (bitwise-ior bits (arithmetic-shift 1 offset)))
       #t))

; Extends the store s with a mapping from the location
; (loc base offset getter setter) to its current value,
; unless s already contains a mapping for this location.
(define (store-add! s base offset getter setter)
  (when (refs-add! (store-refs s) base offset)
    (let ([l (location base offset getter setter)]
          [vals (store-vals s)])
        (set-store-vals! s (cons (cons l (location-ref l)) vals)))))

; Performs the mutation to the storage
; cell at the location (loc base offset getter setter),
; and if this cell has not been mutated before, its
; initial value is added to current-store.
; The getter and setter procedures should read / write
; the cell's value when applied to its base and offset.
(define (store! base offset val getter setter)
  (let ([s (current-store)])
    (when s
      (store-add! s base offset getter setter)))
  ;(printf "store! ~a ~a ~a ~a ~a, ~a\n" base offset val getter setter (current-store))
  (setter base offset val))

; Returns true if the store s is empty.
(define (store-empty? s)
  (zero? (length (store-vals s))))

; Represents the location of a single mutable storage cell.
; A cell location consists of a base object (e.g., a vector)
; and an offset value (e.g., 0) that identifies a
; storage cell within that object. Locations 
; also include getter and setter procedures that can be
; used to read from and write to the referenced cell.
; Two locations are equal? iff their base and offset
; are both eq? to one another.
(struct location (base offset accessor mutator)
  #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc l1 l2 rec-equal?)
     (and (eq? (location-base l1) (location-base l2))
       (eq? (location-offset l1) (location-offset l2))))
   (define (hash-proc l rec-equal-hash)
     (equal-hash-code (cons (eq-hash-code (location-base l)) (eq-hash-code (location-offset l)))))
   (define (hash2-proc l rec-equal-hash2)
     (equal-secondary-hash-code (cons (eq-hash-code (location-base l)) (eq-hash-code (location-offset l)))))])

; Returns the current value stored at the location l.
(define (location-ref l)
  ((location-accessor l) (location-base l) (location-offset l)))

; Stores the value v at the location l.
(define (location-set! l v)
  (store! (location-base l) (location-offset l) v (location-accessor l) (location-mutator l)))

; Rollbacks the contents of all mutated storage cells to their initial 
; values, as given in (current-store), and raises the exception e.
; The current-store is assumed to contain the values that  
; mutated cells held before the current call to with-store.
; This procedure can be called only in the dynamic extent of a
; with-store call.
(define (rollback-exn! e)
  ;(printf "exn: ~a\n" e)
  (for ([lv (store-vals (current-store))])
    (match-define (cons (location base offset _ setter) init) lv)
    (setter base offset init))
  (raise e))

; Rollbacks the contents of all mutated storage cells to their initial 
; values, as given in (current-store), and returns a list of pairs
; that maps a reference to each mutated cell to its current value.
; The current-store is assumed to contain the values that  
; mutated cells held before the current call to with-store.
; This procedure can be called only in the dynamic extent of a
; with-store call.
(define (rollback-capture!)
  ;(printf "capture: ~a\n"  (store-vals (current-store)))
  (for/list ([lv (store-vals (current-store))])
    (match-define (cons (location base offset getter setter) init) lv)
    (define fin (getter base offset))
    (setter base offset init)
    (cons (car lv) fin)))

; The with-store form takes as input an expression, evaluates it,
; and reverts each mutated memory location to its pre-state 
; (i.e., the value it held before the call to with-store).
; 
; If the evaluation of the body terminates normally, (with-store body)
; outputs a result (normal v s) where v is the value computed by the body,
; and s is an association list that maps each mutated location? to its
; post-state (i.e., the value it held after the evaluation of the body).
; In essence, evaluating the body in the current environment has the
; same effect on memory as evaluating (with-store body) and then setting
; the returned memory locations to their post-state value.
; 
; If the evaluation of the body terminates abnormally with an exn:fail?
; exception, with-store reverts all mutated locations to their pre-state
; and re-raises the same exception. 
(define-syntax-rule (with-store body)
  (parameterize ([current-store (make-store)])
    (with-handlers ([exn:fail? rollback-exn!])
      (let ([out body])  
        (normal out (rollback-capture!))))))

; Takes as input a list of n guards and a list of n stores, where
; each store is a list of location/value pairs. For each location l
; occurring in the stores, merge-store mutates l to contain the value
; m = (merge* ... (cons gi vi) ...), where gi = guards[i] and
; vi = stores[i][l] if stores[i] has a binding for l; otherwise, 
; vi = (location-ref l). The procedure assumes that no store contains a
; duplicate binding for any location.
; 
; This store merging procedure is correct under the assumption that 
; (1) the guards are disjoint under all models (i.e., at most one 
; is ever true), and (2) the verification conditions force at least 
; one guard to be true under all models that satisfy both the
; asserts and the assumes. 
(define (merge-stores! guards stores)
  (match stores
    [(list (list) ...)   (void)]                    ; Nothing to merge.
    [(list s) (for ([lv s])                         ; If given only one store, just apply its effects 
                (location-set! (car lv) (cdr lv)))] ; since its guard must be true under the current spec.
    [_ (define hash-stores (map make-hash stores))
       (for ([lv (remove-duplicates (apply append stores) equal? #:key car)])
         (define loc (car lv))
         (define val (location-ref loc))
         (location-set! loc
            (apply merge*
                   (for/list ([g guards] [hs hash-stores])
                     (cons g (if (hash-has-key? hs loc)
                                 (hash-ref hs loc)
                                 val))))))]))  
  





