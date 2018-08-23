#lang racket

(require "../data.rkt" "../record.rkt" "renderer.rkt")
(provide make-noop-renderer)

; The noop renderer does nothing!

(define (make-noop-renderer source name [options (hash)])
  (noop-renderer source name))

(struct noop-renderer (source name) 
  #:transparent
  #:methods gen:renderer
  [(define start-renderer void)
   (define (finish-renderer self profile)
     (printf "Profiled ~v events.\n" (length (unbox (profile-state-events profile))))
     (define types (make-hash))
     (for ([e (unbox (profile-state-events profile))])
       (define type (vector-ref (struct->vector e) 0))
       (hash-set! types type (add1 (hash-ref types type 0))))
     (for ([type (sort (hash-keys types) > #:key (lambda (t) (hash-ref types t)))])
        (printf "* ~a: ~v\n" type (hash-ref types type))))])
