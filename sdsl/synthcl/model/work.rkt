#lang rosette


(provide current-work-size current-global-id 
         in-host? in-kernel?
         get_work_dim get_global_offset 
         get_global_size get_local_size get_num_groups
         get_global_id get_local_id get_group_id)

; A valid work size specification consists of three lists of size k,
; where 1 <= k <= 3.  The first list represents the global work offset, 
; the second is the global work size, and the third is the local work 
; size.  The offset list must contain only integers, and the work size 
; lists must be positive integers.  Additionally, the ith element of the 
; global work size list must be evenly divisible by the ith element of the 
; local work size list.  See p. 171 of opencl-1.2 specification.
; 
; The global offset and work size may be left empty when initializing the 
; current-work-size parameter.  If the offset is empty, the zero offset is 
; used.  If the local work size is empty, a work size is picked by the 
; implementation.
(define current-work-size
  (make-parameter '(()()())
   (lambda (ws)
     (match ws  
       [(list (? list? off) (and (? list? gws) (app length n)) (? list? lws))
        (unless (and (<= 1 n) (<= n 3) (andmap positive-integer? gws))
          (raise-argument-error 
           'current-work-size 
           "a valid global work size specification" gws))
        (let* ([off (cond [(null? off) (make-list n 0)]
                          [(= (length off) n) off]
                          [else (raise-argument-error 
                                 'current-work-size 
                                 "a valid global work offset specification" off)])]
               [lws (cond [(null? lws) (make-list n 1)]
                          [(and (= (length lws) n) 
                                (andmap positive-integer? lws) 
                                (andmap (compose1 zero? remainder) gws lws)) lws]
                          [else (raise-argument-error 
                                 'current-work-size 
                                 "a valid local work size specification" ws)])])
          (list off gws lws))]       
       [_ (raise-argument-error 
           'current-work-size 
           "a valid work size specification" ws)]))))

        
; The unique global ID that is in use by the currently running kernel, 
; including the offset. The ID is a list of k integers, where k is 
; (get_work_dim) and the ith integer is between (get_global_offset i), 
; inclusive, and (get_global_offset i) + (get_global_size i), exclusive.
; If the currently running code is the host rather than a kernel, then 
; the global_id is the empty list.
;
; The current-global-id parameter can be initialized either with a list 
; of k integers representing a valid ID, or with an integer between 0,
; inclusive, and n, exclusive, where n = (get_global_size 0) * ... *
; (get_global_size (- 1 (get-work-dim))).  This integer is converted to 
; the list representation.
(define current-global-id
  (make-parameter '()
   (lambda (id)
     (define ws (current-work-size))
     (define off (offset ws))
     (define gws (global ws))
     (define d (length gws))
     (define n (apply * gws))
     (cond [(and (list? id) (= (length id) d) 
                 (andmap integer? id) 
                 (andmap <= off id)
                 (andmap < id (map + off gws)))
            id]
           [(and (integer? id) (<= 0 id) (< id n))
            (map + off
                 (let loop ([gws gws][n n][r id])
                   (if (null? gws)
                       '()
                       (let* ([n (quotient n (car gws))])
                         (cons (quotient r n)
                               (loop (cdr gws) n (remainder r n)))))))]
           [else (raise-argument-error 
                   'current-global-id 
                   "valid global ID for current work_size" id)]))))

; Returns true iff x is a positive integer.
(define (positive-integer? x)
  (and (integer? x) (positive? x)))

(define-values (offset global local) (values first second third))

; Returns true iff the currently running code is being executed 
; by the host.
(define (in-host?) (null? (current-global-id)))

; Returns true iff the currently running code is being executed 
; by a kernel host.
(define (in-kernel?) (not (null? (current-global-id))))

; Returns the offset or number of global or local work-items
; specified for dimension identified by dimindx. This value is 
; given by the global_work_offset, global_work_size or 
; local_work_size argument to clEnqueueNDRangeKernel. Valid 
; values of dim are 0  to get_work_dim() – 1. For other values 
; of dim, get_work_size() returns the provided default value. 
(define (get_work_size which dim default)
  (let ([ws (which (current-work-size))])
    (if (and (<= 0 dim) (< dim (length ws)))
        (list-ref ws dim)
        default)))

; Returns the number of dimensions in use. This is the 
; value given to the work_dim argument specified in 
; clEnqueueNDRangeKernel. 
; See Ch. 6.12.1 of opencl-1.2 specification.
(define (get_work_dim) 
  (length (offset (current-work-size))))

; Returns the offset values specified in global_work_offset argument 
; to clEnqueueNDRangeKernel. Valid values of
; dim are 0 to get_work_dim() – 1. For other values,
; get_global_offset() returns 0.
; See Ch. 6.12.1 of opencl_1.2 specification.
(define (get_global_offset dim) 
  (get_work_size offset dim 0))

; Returns the number of global work-items specified 
; for dimension identified by dim. This value is 
; given by the global_work_size argument to 
; clEnqueueNDRangeKernel. Valid values of dimindx are 0 
; to get_work_dim() – 1. For other values of dim, 
; get_global_size() returns 1. 
; See Ch. 6.12.1 of opencl-1.2 specification.
(define (get_global_size dim) 
  (get_work_size global dim 1))

; Returns the number of local work-items specified in dimension
; identified by dim. This value is given by the local_work_size
; argument to clEnqueueNDRangeKernel. Valid values
; of dim are 0 to get_work_dim() – 1. For other values of dim,
; get_local_size() returns 1.
; See Ch. 6.12.1 of opencl-1.2 specification.
(define (get_local_size dim) 
  (get_work_size local dim 1))

; Returns the number of work-groups that will execute a kernel for
; dimension identified by dim. Valid values of dim are 0 to
; get_work_dim() – 1. For other values of dimindx, get_num_groups ()
; returns 1.
; See Ch. 6.12.1 of opencl-1.2 specification.
(define (get_num_groups dim)
  (/ (get_global_size dim) (get_local_size dim)))

; Returns the unique global work-item ID value for dimension 
; identified by dim. The global work-item ID specifies the 
; work-item ID based on the number of global work-items specified to 
; execute the kernel. Valid values of dim are 0 to 
; get_work_dim() – 1. For other values of dim, get_global_id() 
; returns 0.
; See Ch. 6.12.1 of opencl-1.2 specification.
(define (get_global_id dim)
  (let ([gid (current-global-id)])
    (if (and (<= 0 dim) (< dim (length gid))) 
        (list-ref gid dim)
        0)))

; Returns the unique local work-item ID i.e. a work-item within a
; specific work-group for dimension identified by dim. Valid values
; of dim are 0 to get_work_dim() – 1. For other values of dim,
; get_local_id() returns 0.
; See Ch. 6.12.1 of opencl-1.2 specification.
(define (get_local_id dim)
  (let ([gid (get_global_id dim)]
        [lws (get_local_size dim)]
        [off (get_global_offset dim)])
    (remainder (max 0 (- gid off)) lws)))

; Returns the work-group ID which is a number from 0 ..
; get_num_groups(dim) – 1. Valid values of dim are 0 to
; get_work_dim() – 1. For other values, get_group_id() returns 0.
; See Ch. 6.12.1 of opencl-1.2 specification.
(define (get_group_id dim)
  (let ([gid (get_global_id dim)]
        [lws (get_local_size dim)]
        [off (get_global_offset dim)])
    (quotient (- gid off) lws)))
