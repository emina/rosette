#lang rosette

(require rackunit rackunit/text-ui (rename-in rackunit [check-exn rackunit/check-exn])
         rosette/lib/roseunit
         "../model/memory.rkt" "../model/work.rkt" 
         "../model/reals.rkt"  "../model/pointers.rkt"
         "../model/context.rkt" "../model/buffer.rkt" "../model/flags.rkt")

(define (read-write-only-buffer)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (clCreateBuffer (current-context)  CL_MEM_WRITE_ONLY 10))
    (parameterize ([current-global-id '(9)])
      (pointer-ref ptr 1))))

(define (write-read-only-buffer)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (clCreateBuffer (current-context)  CL_MEM_READ_ONLY 10))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 1 3))))

(define (write-write-conflict)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 1 100))
    (parameterize ([current-global-id '(3)])
      (pointer-set! ptr 1 100))))

(define (read-write-conflict)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 9 100))
    (parameterize ([current-global-id '(3)])
      (pointer-ref ptr 9))))

(define (successful-synchronize)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 9 100))
    (parameterize ([current-global-id '(3)])
      (pointer-set! ptr 1 100))
    (memory-synchronize! (context-global-memory (current-context)))
    (parameterize ([current-global-id '(4)])
      (pointer-ref ptr 1))
    (check-equal? (pointer-ref ptr 1) 100)
    (check-equal? (pointer-ref ptr 9) 100))) 

(define (vector-write-type-error)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 0 (int2 4 5)))))

(define (vector-write-range-error)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 3 (int3 4 5 6)))))

(define (vector-write-success)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 6) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 1 (int3 4 5 6)))
    (pointer->list ptr)))

(define (vector-read-type-error)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 0 (int3 4 5 6))
      (int (pointer-ref ptr 0)))))

(define (vector-read-range-error)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-ref ptr 3))))

(define (vector-read-success)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 9) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 1 (int3 4 5 6))
      (pointer-ref ptr 1))))

(define (vector-write-read-conflict-error)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 0 (int3 1 2 3)))
    (parameterize ([current-global-id '(3)])
      (pointer-ref ptr 0))))

(define (vector-write-read-success)
  (parameterize ([current-context (clCreateContext)]
                 [current-work-size '((0) (10) (1))])
    (define ptr (pointer-cast (clCreateBuffer (current-context)  CL_MEM_READ_WRITE 10) int3))
    (parameterize ([current-global-id '(9)])
      (pointer-set! ptr 0 (int3 1 2 3)))
    (parameterize ([current-global-id '(3)])
      (pointer-ref ptr 2))))

       
(define-syntax-rule (check-exn e ...)
  (begin
    (rackunit/check-exn e ...)
    (clear-vc!)))

(define memory-tests
  (test-suite+ 
   "Tests for memory functions"

   (check-exn #px"pointer-ref: cannot read from a write-only memory address #x0\\[1\\]"
              read-write-only-buffer)
   (check-exn #px"pointer-set!: cannot write to a read-only memory address #x0\\[1\\]" 
              write-read-only-buffer)
   (check-exn #px"pointer-set!: access conflict detected on memory address #x0\\[1\\]"
              write-write-conflict)
   (check-exn #px"pointer-ref: access conflict detected on memory address #x0\\[9\\]"
              read-write-conflict)
   (check-exn #px"implicit-conversion: cannot convert #\\(4 5\\) to int3"
              vector-write-type-error)
   (check-exn #px"take" 
              vector-write-range-error)
   (check-equal? (vector-write-success) '(0 0 0 4 5 6))
   (check-exn #px"int: contract violation"
              vector-read-type-error)
   (check-exn #px"take" 
              vector-read-range-error)
   (check-equal? (vector-read-success) (int3 4 5 6))
   (check-equal? (vector-write-read-success) (int3 0 0 0))
   (check-exn #px"pointer-ref: access conflict detected on memory address #x0\\[0\\]"
              vector-write-read-conflict-error)
   (check-not-exn successful-synchronize)))

(time (run-tests memory-tests))
