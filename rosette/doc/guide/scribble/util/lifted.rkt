#lang racket

(provide select rosette-evaluator)

(require 
  (for-label racket racket/generic)
  (only-in rosette rosette union union-contents union?)
  racket/sandbox
  (only-in scribble/manual elem racket))

(define lifted? 
  (let ([lifted (apply set (rosette))])
    (lambda (id) (set-member? lifted id))))

(define (select racket-ids)
   (apply elem 
          (add-between (map (lambda (id) (racket #,#`#,id)) 
                            (filter lifted? racket-ids)) ", ")))

(define (rosette-printer v)
  (match v
    [(? void?) (void)]
    [(? custom-write?) 
     ((custom-write-accessor v) v (current-output-port) 1)]
    [(? pair?) (printf "'~a" v)]
    [(? null?) (printf "'()")]
    [(? symbol?) (printf "'~a" v)]
    [_  (printf "~a" v)]))

(define (rosette-evaluator [eval-limits #f])
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-path-permissions `((execute ,(byte-regexp #".*")))]
                  [sandbox-memory-limit #f]
                  [sandbox-eval-limits eval-limits]
                  [current-print rosette-printer])
     (make-evaluator 'rosette/safe)))

                    