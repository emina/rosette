#lang racket

(require racket/syntax racket/splicing )
(provide define-array array-procedure reshape split-at* list-ref*)

; Provides a macro for defining multidimensional arrays.  The 
; form (define-array var dims vals) defines a multidimensional 
; array from a dimension specification and a flat list of values.  The macro 
; introduces a name transformer var.  When used as an identifer
; the var transformer returns the array in the form of a list of 
; lists.  When used as a function, it takes a sequence of indices
; and returns the element (or a subarray) at the specified position. 
; 
; The dimension specification should be list of positive natural 
; numbers.  For example, '(3 2 3) specifies a 3x2x3 array.  The 
; vals list should contain exactly (apply * dims) values.
;
; The form (define-array var vals) assumes that vals is already a 
; list of lists.  It simply introduces a name transformer for var, 
; as described above.
(define-syntax (define-array stx)
  (syntax-case stx ()
    [(_ id dims vals) 
      #`(define-array id (reshape dims vals))]
    [(_ id vals) 
     #`(splicing-let ([array (array-procedure vals)])
         (define-syntax id
           (syntax-id-rules (set!)
             [(set! id e) 
              (error 'set! "cannot modify an immutable reference: ~s" (syntax->datum #'id))]
             [(id idx (... ...)) (array idx (... ...))]
             [id (array)])))]))

; This macro expands to a procedure wrapper that allows
; the elements in the given nested list representation of 
; an array to be accessed using a call of the form (vals idx ...).
; Applying the resulting procedure to no arguments yields the 
; entire array (that is, list of lists).
(define-syntax-rule (array-procedure vals)
  (let ([array vals])
    (procedure-rename
     (lambda pos
       (apply list-ref* array pos))
     (string->symbol (format "array~aD" (length array))))))

; This function returns a nested list representation 
; of the given flat list using the given shape specification.
; The shape specification is a flat list of positive natural 
; numbers.  For example, '(3 2) specifies a nested list that 
; corresponds to a 3x2 array in row major order, i.e., 
; (reshape '(3 2) '(0 1 2 3 4 5)) yields  '((0 1 2) (3 4 5)).  
; The behavior of this function is unspecified if the length of 
; the vals list is not exactly (apply * dims).   
(define (reshape dims vals)
  (cond [(null? dims) null]
        [(null? (cdr dims)) vals] 
        [else (let ([rest (cdr dims)])
                (map (curry reshape (cdr dims)) (split-at* vals (apply * rest))))]))

; Splits a list of size k*n into k sublists of size n.  The 
; sublists are returned in a list.  The behavior of this function 
; is unspecified if the length of the list is not a multiple of n.  
(define (split-at* vals n)
  (if (null? vals) 
      null
      (let-values ([(left right) (split-at vals n)])
        (cons left (split-at* right n)))))

; Returns the value in the given nested list representation of a 
; mulitdimensional array that is at the specified position.  The 
; value itself may be a list; for example, (list-ref* '((0 1) (2 3)) 0)
; produces '(0 1) while (list-ref* '((0 1) (2 3)) 1 0) produces 2.
(define (list-ref* vals . pos)
  (if (null? pos)
      vals
      (apply list-ref* (list-ref vals (car pos)) (cdr pos))))
