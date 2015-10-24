#lang racket

(provide current-oracle oracle? (rename-out [make-oracle oracle]))

#|--------------current state parameters--------------|#

(struct oracle ([tbl])
  #:property prop:procedure
  (lambda (self var)
    (let* ([vars (oracle-tbl self)]
           [choice-idx (hash-ref vars var 0)])
      (hash-set! vars var (+ choice-idx 1))
      choice-idx))
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "oracle~a" (oracle-tbl self)))])

(define make-oracle
  (case-lambda 
    [() (oracle (make-hash))]
    [(other) (oracle (hash-copy (oracle-tbl other)))]))

(define current-oracle
  (make-parameter (make-oracle)
                  (lambda (oracle)
                    (unless (oracle? oracle)
                      (error 'current-oracle "expected an oracle procedure, given ~s" oracle))
                    oracle)))

