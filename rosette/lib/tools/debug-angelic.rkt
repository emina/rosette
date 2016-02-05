#lang s-exp rosette

(require (only-in racket filter) rosette/query/debug)

(provide debug* debug+)

(define-syntax-rule (debug+ [rel] 
                            #:guarantee validator 
                            #:failing bad 
                            #:passing good ...)
  (parameterize ([relax? rel])
    (debug* (curry apply validator) bad good ...)))

(define (debug* validator bad . good)
  (let loop ([candidates (core (debug (validator bad)))];(candidates validator bad)]; 
             [good good])
    (if (or (null? candidates) (null? good))
        (unsat candidates)
        (loop (filter (curryr flexible? validator (car good)) candidates) (cdr good)))))

(define (flexible? expr validator input)
  (define indicator (relaxed-by expr))
  (and indicator 
       (with-handlers ([exn:fail? (const #f)])
         (parameterize ([current-oracle (oracle)]
                        [relate (lambda (relaxer val)
                                  (if (term=? relaxer indicator)
                                      (not (equal? relaxer val))
                                      (equal? relaxer val)))])
           (solve (validator input))
           #t))))

(define (candidates validator bad)
  (parameterize ([current-oracle (oracle)])
    (let-values ([(out asserts) (with-asserts (validator bad))])
      (filter (curryr flexible? validator bad) asserts))))
      