#lang racket

(provide current-oracle oracle)

#|--------------current state parameters--------------|#

(define (oracle)
  (let ([vars (make-hash)])
    (procedure-rename 
     (lambda (var)
      (let ([choice-idx (hash-ref vars var 0)])
        (hash-set! vars var (+ choice-idx 1))
        choice-idx))
     'oracle)))

(define current-oracle
  (make-parameter (oracle)
                  (lambda (oracle)
                    (unless (procedure? oracle)
                      (error 'current-oracle "expected an oracle procedure, given ~s" oracle))
                    oracle)))

