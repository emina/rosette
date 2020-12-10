#lang rosette

(require "errors.rkt" "work.rkt" "type.rkt" "reals.rkt" 
         (only-in "pointers.rkt" gen:pointer)           
         racket/generic rosette/lib/match)

(provide (rename-out [make-memory memory]) memory? memory-empty?
         memory-synchronize! memory-allocate! memset memory-owns? raw-pointer? NULL)

; Creates a fresh, empty memory with the given capacity 
; or 64 if no capacity is given.
(define (make-memory [capacity 512])
  (assert (and (integer? capacity) (positive? capacity))
          (format "memory: contract violation\n  expected: positive capacity\n  given: ~a" capacity))
  (memory 0 (make-vector capacity)))

; Represents a memory structure that contains zero or more allocated
; regions in its heap vector.  The address field indicates the next 
; free address in the memory.  The addressing is two dimensional:  the 
; address of a memory cell pointed to by a pointer p is (m, n), where 
; m is the base address of p in memory and n is the index of the cell 
; within p's contents. 
;
; Our model of pointers (and memory) is simplified in that we assume 
; all scalar datatypes have the same size (in bytes).  We also check that 
; all accesses to memory by entities with different current-global-id's are 
; non-conflicting.  
(struct memory ([address #:mutable] [heap #:mutable]) 
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf 
      port 
      "#memory~a" (memory-heap self)))])

; Returns true if the given memory is empty.
(define (memory-empty? m)
  (= 0 (memory-address m)))

; Returns a new pointer to a region with the given 
; number of cells, allocated from the given memory. 
; The optional scalar type argument, if given, specifies
; the type of the data stored in a single cell of the 
; allocated region. Default is #f (any scalar type).
(define (memory-allocate! mem size [type #f])
  (when type (assert (scalar-type? type)))
  (define address (memory-address mem))
  (define heap (memory-heap mem))
  (when (>= address (vector-length heap))
    (define heapX2 (make-vector (* 2 (vector-length heap))))
    (vector-copy! heapX2 0 heap)
    (set-memory-heap! mem heapX2)
    (set! heap heapX2))
  (define contents (make-vector size))
  (define writes (make-vector size #f))
  (define ptr (raw-pointer address contents writes type))
  (vector-set! heap address ptr)
  (set-memory-address! mem (add1 address))
  ptr)

; Ensures that all reads and writes to cells in the given 
; memory are performed before this procedure returns.
(define (memory-synchronize! mem)
  (define heap (memory-heap mem))
  (for ([address (in-range 0 (memory-address mem))])
    (vector-map! (const #f) 
                 (raw-pointer-writes (vector-ref heap address)))))

; Returns true iff the given raw pointer points to an allocated 
; region in the given memory.
(define (memory-owns? mem ptr)
  (match* (mem ptr)
    [((? memory?) (== NULL)) #t]
    [((memory maddr heap) (raw-pointer paddr contents _ _))
     (and (< paddr maddr) 
          (raw-pointer-contents (vector-ref heap paddr)) contents)]
    [(_ _) #f]))

; Sets the first num locations pointed to by the given pointer to the 
; given value.  The pointer's type is assumed (but not checked) to be void*.
(define (memset ptr val num)
  (match ptr
    [(raw-pointer address contents writes _)
     (check-conflicts 'memset address writes 0 num)
     (vector-copy! writes   0 (make-vector num (current-global-id)))
     (vector-copy! contents 0 (make-vector num val))
     ptr]))
  
; Returns a view of the given pointer that treats its contents 
; as having the specified type T.  In particular, 
; pointer-ref and pointer-set! operations on the returned pointer 
; will produce and consume only values of type T. For example,
; (pointer-set! p i v) to a pointer p that has been cast to the 
; int3 type will implicitly convert t to int3 before performing 
; the update.  Similarly, (pointer-ref p i) will return a value of 
; type int3.
(define (raw-pointer-cast ptr type)
  (match ptr
    [(raw-pointer address contents writes _)
     (raw-pointer address contents writes type)]
    [_ (raise-pointer-cast-error ptr type)]))

; Returns a list view of the storage region for the given pointer.
(define (raw-pointer->list ptr)
  (match ptr
    [(raw-pointer _ contents _ _) (vector->list contents)]
    [_ (raise-argument-error 'pointer->list "pointer" ptr)]))

; Returns the length of the region pointed to by the given pointer.
(define (raw-pointer-size ptr)
  (vector-length (raw-pointer-contents ptr)))
 
; Checks that there are no access conflicts on the memory cell (address, idx).
(define (check-conflicts caller address writes idx len)
  (when writes 
    (for ([i len] [w (take (drop (vector->list writes) idx) len)])
      (assert (or (false? w) (equal? w (current-global-id)))
              (format "~a: access conflict detected on memory address #x~x[~a]" caller  
                      address (+ idx i))))))
    
; Returns the value stored at the given offset from the 
; base address of the given pointer (i.e., ptr[idx]).
(define (raw-pointer-ref ptr idx)
  (match ptr
    [(raw-pointer address contents writes type)
     (match type
       [(and (? vector-type?) (app real-type-length len))
        (define offset (* idx len))
        (check-conflicts 'pointer-ref address writes offset len)
        (define base (type-base type))       
        (apply type (map (base) (take (drop (vector->list contents) offset) len)))]
        ;(apply type (for/list ([i len]) ((base) (vector-ref contents (+ offset i)))))]
       [_
        (check-conflicts 'pointer-ref address writes idx 1)
        (let ([val (vector-ref contents idx)])
          (if type ((type) val) val))])]
    [_ (raise-argument-error 'pointer-ref "pointer" ptr)]))

; Sets the memory cell at the given offset from the 
; base address of the given pointer to the given value 
; (i.e., ptr[idx] := val).
(define (raw-pointer-set! ptr idx val)
  (match ptr
    [(raw-pointer address contents writes type)
     (match type
       [(and (? vector-type?) (app real-type-length len))
        (define offset (* idx len))
        (check-conflicts 'pointer-set! address writes offset len)
        (vector-copy! writes offset (make-vector len (current-global-id)))
        (vector-copy! contents offset ((type) val))]
        ;(define v ((type) val))
        ;(for ([i len])
        ;  (vector-set! writes (+ offset i) (current-global-id))
        ;  (vector-set! contents (+ offset i) (vector-ref v i)))]
       [_
        (check-conflicts 'pointer-set! address writes idx 1)
        (vector-set! writes idx (current-global-id))
        (vector-set! contents idx (if type ((type) val) val))])] 
    [_ (raise-argument-error 'pointer-set! "pointer" ptr)]))

; Each pointer keeps track of the global identifiers of 
; work items that write to each location in the pointer's array.  
; A pointer can also have an optional type field, which 
; specifies the type of the elements it contains.  This field, 
; if not #f, determines the behavior of ptr-ref and 
; ptr-set! operations. 
(struct raw-pointer (address contents writes type)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "#x~x" (raw-pointer-address self))
     (case mode
       [(#t) (write (raw-pointer-contents self) port)]
       [(#f) (display (raw-pointer-contents self) port)]
       [else (fprintf port "~a" (raw-pointer-contents self))]))]
  #:methods gen:pointer
  [(define (pointer-address self) (raw-pointer-address self))
   (define pointer-size raw-pointer-size)
   (define pointer-cast raw-pointer-cast)
   (define pointer-ref raw-pointer-ref)
   (define pointer-set! raw-pointer-set!)
   (define pointer->list raw-pointer->list)])

; The null pointer.
(define NULL (raw-pointer 0 (vector) (vector) #f))
