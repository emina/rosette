#lang racket

(require (only-in "../../config/config.rkt" configured) (only-in "../../base/enum.rkt" enums enum-<?))

(provide finitize define-encoder enum-comparison-op?)

(define (finitize n) 
  (let ([bw (configured bitwidth)])
    (match n
      [+inf.0 (let ([out (sub1 (arithmetic-shift 1 (sub1 bw)))]) 
                (log-info "cannot represent ~a exactly; truncating to ~a" n out)
                out)]
      [-inf.0 (let ([out (- (arithmetic-shift 1 (sub1 bw)))]) 
                (log-info "cannot represent ~a exactly; truncating to ~a" n out)
                out)]
      [_ (let* ([mask (arithmetic-shift -1 bw)]
                [masked (bitwise-and (bitwise-not mask) (inexact->exact (round n)))]
                [out (if (bitwise-bit-set? masked (- bw 1))
                         (bitwise-ior mask masked)  
                         masked)]) ;(printf "bw=~a, mask=~b, masked=~b, out=~b\n" bw mask masked out)
           (unless (= n out)
             (log-info "cannot represent ~a with ~a bits; truncating to ~a" n bw out))
           out)])))

(define-syntax define-encoder
  (syntax-rules ()
    [(_ id [#:== [rosette-op solver-op1] ...] [#:?  [? solver-op2] ...])
     (define (id op) 
       (cond [(eq? op rosette-op) solver-op1] ... 
             [(? op) solver-op2] ...
             [else #f]))]
    [(_ id [rosette-op solver-op1] ...)
     (define-encoder id [#:== [rosette-op solver-op1] ...] [#:? ])]))

(define (enum-comparison-op? op)
  (for/or ([e enums]) (eq? op (enum-<? e))))

