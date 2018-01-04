#lang racket

(require 
 (for-syntax racket) "../util/ord-dict.rkt")

(provide speculate speculate* apply! location=? (rename-out [state-val location-final-value]))

; The env parameter stores an eq? based hash-map which we use to keep
; track of boxes, vectors and structs that are mutated.  
(define env (make-parameter #f))

; The speculate expression takes the form (speculate body), where body is
; an expression.  A speculate call produces two values:  the value that the
; body would produce if executed in the current environment, and a closure 
; that stores a representation of all state updates that the execution of 
; body would make.  The closure accepts a two argument function f, and 
; applies encapsulated state updates so that each updated location is set  
; to (f v body-v), where body-v is the final value the body would assign to v.  
;
; Any exceptions thrown by body are caught, all updates are rolled-back without 
; encapsulating the final states, and the result of speculate is (values #f #f).
(define-syntax-rule (speculate body) 
  ; using an eq? rather than equal? hash map to manage the environment bindings
  ; is critical for mutable objects whose hash code may change upon mutation.  note 
  ; that variables are keyed by the symbol representing their name, so eq? comparisons
  ; for them are equivalent to equal? comparisons.
  (parameterize ([env (odict null eq?)]) 
    ; roll-back state updates, encapsulate
    ; updates to set! variables as specified above, 
    ; and return the value of the body together with the 
    ; encapsulation of the state changes
    (with-handlers ([exn:fail? rollback/suppress])
      (values body (rollback/encapsulate)))))

; The speculate* expression takes the form (speculate* body), where body is
; an expression.  A speculate* call produces two values:  the value that the
; body would produce if executed in the current environment, and a list of 
; locations, each of which encapsulates the pre and post state of a location 
; mutated during the execution of the body.  The returned locations can be 
; compared with location=?.  
;
; Each encapsulated update acts as a procedure that accepts a two-argument 
; function f.  The location for the encapsulated updated is then set to 
; (f v body-v), where body-v is the final value the body would assign to the 
; location and v is the current value in that location. The procedure 
; (location-final-value loc) can be used to obtain the final value that the 
; body would assign to a given location.
;
; Any exceptions thrown by body are caught, all updates are rolled-back without 
; encapsulating the final states, and the result of speculate is (values #f #f).
(define-syntax-rule (speculate* body) 
  ; using an eq? rather than equal? hash map to manage the environment bindings
  ; is critical for mutable objects whose hash code may change upon mutation.  note 
  ; that variables are keyed by the symbol representing their name, so eq? comparisons
  ; for them are equivalent to equal? comparisons.
  (parameterize ([env (odict null eq?)]) 
    ; roll-back state updates, encapsulate
    ; updates to set! variables as specified above, 
    ; and return the value of the body together with the 
    ; encapsulation of the state changes
    (with-handlers ([exn:fail? rollback/suppress])
      (values body (rollback/collect)))))

; A function that handles calls to structure mutators.  
(define apply! 
  (case-lambda 
    [(setter getter receiver key val)
     (record! receiver key getter setter)
     (setter receiver key val)]
    [(setter getter receiver val) 
     (record! receiver setter getter setter)
     (setter receiver val)]))

; Stores the state of a mutation to the location in a given receiver, 
; together with getters and setters that can be used to read/write 
; the mutated location. The val field stores the value that was read 
; from the location at some point in time (e.g., beginning/end of 
; speculation).  The attached procedure accepts a two argument function f 
; and sets the encapsulated location to (f (getter) val).
(struct state (receiver location val getter setter) 
  #:transparent
  #:property prop:procedure 
  (lambda (self proc) 
    (let ([receiver (state-receiver self)]
          [location (state-location self)]
          [getter (state-getter self)]
          [setter (state-setter self)])
      (record! receiver location getter setter) 
      (cond [(dict? receiver) 
             (setter receiver location (proc (getter receiver location) (state-val self)))]
            [else ; struct or box
             (setter receiver (proc (getter receiver) (state-val self)))]))))

(define (get getter receiver location)
  (cond [(dict? receiver) (getter receiver location)]
        [else (getter receiver)]))

(define (state-rollback! s)
  (let ([receiver (state-receiver s)]
        [location (state-location s)]
        [getter (state-getter s)]
        [setter (state-setter s)])
    (cond [(dict? receiver) 
           (setter receiver location (state-val s))]
          [else ; struct or box
           (setter receiver (state-val s))])))

; Returns true iff both objects encapsulate updates to the same location.
(define (location=? s0 s1)
  (match* (s0 s1)
    [((state rec0 loc0 _ _ _) (state rec1 loc1 _ _ _))
     (and (eq? rec0 rec1) (equal? loc0 loc1))]
    [(_ _) #f]))

 
; Adds a record of the given variable's or object's current state 
; to the environment, if the environment is valid and does not 
; already have a mapping for the record!-ed variable or object.
(define-syntax-rule (record! obj location getter setter)
  (when (and (env)
             (not (env-has-state? obj location))) ; we do this check separately so that the getter/setter  
    (env-set! obj location getter setter)))       ; lambdas don't get created unless they are needed

; Returns a true value if the current environment (assumed not be #f)
; has a state record for the given mutation receiver and location of 
; mutation. For structs, the location is the field-setter function for 
; the mutated field.  For dictionary objects, the location is the key within the 
; dictionary to which the dict-set! operation is being applied. For boxes, 
; the location is the set-box! procedure.
(define (env-has-state? receiver location)
  (let ([env (env)])
    (and (dict-has-key? env receiver)                     ; compound object 
         (dict-has-key? (dict-ref env receiver) location))))

; Augments env with a mapping from the given receiver to a state record reflecting 
; the current state at the given location, as obtained by the given getter 
; procedure. This function assumes that (env-has-state? receiver location) is false.
(define (env-set! receiver location getter setter)
  (let ([env (env)]
        [new-state  (state receiver location (get getter receiver location) getter setter)])
    (let ([locations (dict-ref! env receiver make-hash)]) ; compound object
      (dict-set! locations location new-state))))

; Reverts the state of set! variables and struct fields to
; their initial values, without encapsulating the final state updates.
; Returns (values #f #f).  The error argument is ignored.
(define (rollback/suppress err)
  ;(printf "\n\nERROR: ~a\n\n" err)
  (unless (zero? (dict-count (env)))
    (for* ([states (in-dict-values (env))]
           [s (if (list? states) (in-list states) (in-dict-values states))])
      (state-rollback! s)))    ; roll-back
  (values #f #f))
       
; Reverts the state of set! variables and struct fields to
; their initial values, and returns an encapsulation of 
; the final state updates.
(define (rollback/encapsulate)
  (if (zero? (dict-count (env)))
      void
      (let ([updates (rollback/collect)])
        (lambda (proc)
          (for ([s (in-list updates)])
            (s proc))))))

; Reverts the state of set! variables and struct fields to
; their initial values, and returns a list that contains a 
; copy of the final state of each location bound in the current 
; environment.  
(define (rollback/collect)
  (for*/list ([states (in-dict-values (env))]
              [s (if (list? states) (in-list states) (in-dict-values states))])
    (let ([final (get (state-getter s) (state-receiver s) (state-location s))])
      (state-rollback! s)                  ; roll-back
      (struct-copy state s [val final])))) ; collect final states

