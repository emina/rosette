#lang racket

(provide with-store store! location? location-ref location-set!)

; The current-store parameter contains a store that 
; maps (abstract) memory locations to values.  Each mapped
; location identifies a storage cell that has been the mutated
; via a store! call in the dynamic extent of a call to
; speculate. The store maps each such location to the value
; that was stored at that location before the current call 
; to speculate.
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
; to the current call to speculate.
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
    (let ([l (location base offset getter setter)])
        (set-store-vals! s (cons l (location-ref l))))))

; Performs the mutation to the storage
; cell at the location (loc base offset getter setter),
; and if this cell has not been mutated before, its
; initial value is added to current-store.
; The getter and setter procedures should read / write
; the cell's value when applied to its base and offset.
(define (store! base offset getter setter val)
  (let ([s (current-store)])
    (when s
      (store-add! s base offset getter setter)))
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
  (store! (location-base l) (location-offset l) (location-accessor l) (location-mutator l)))

; Rollbacks the contents of all mutated storage cells to their initial 
; values, as given in (current-store), and raises the exception e.
; The current-store is assumed to contain the values that  
; mutated cells held before the current call to speculate.
; This procedure can be called only in the dynamic extent of a
; speculate call.
(define (rollback-exn! e)
  (for ([lv (store-vals (current-store))])
    (match-define (cons (location base offset _ setter) init) lv)
    (setter base offset init))
  (raise e))

; Rollbacks the contents of all mutated storage cells to their initial 
; values, as given in (current-store), and returns a list of pairs
; that maps a reference to each mutated cell to its current value.
; The current-store is assumed to contain the values that  
; mutated cells held before the current call to speculate.
; This procedure can be called only in the dynamic extent of a
; speculate call.
(define (rollback-capture!)
  (for/list ([lv (store-vals (current-store))])
    (match-define (cons (location base offset getter setter) init) lv)
    (define fin (getter base offset))
    (setter base offset init)
    (cons (car lv) fin)))

; The with-store form takes as input an expression, evaluates it,
; and reverts each mutated memory location to its pre-state,
; i.e., the value it held before the call to with-store.
; 
; If the evaluation of the body terminates normally, with-store outputs a pair
; of two values: (1) the result of evaluating the body, and (2) an association list
; that maps each mutated location? to its post-state, i.e., the value it held after
; the evaluation of the body. In essence, evaluating the body in the current environment
; has the same effect on memory as evaluating (with-store body) and then setting the
; returned memory locations to their post-state value.
; 
; If the evaluation of the body terminates abnormally with an exn:fail? exception,
; with-store reverts all mutated locations to their pre-state and re-raises the
; same exception. 
(define-syntax-rule (with-store body)
  (parameterize ([current-store (make-store)])
    (with-handlers ([exn:fail? rollback-exn!])
      (let ([out body])
        (cons out (rollback-capture!))))))
    





