#lang rosette

(define (sum xs)
  (cond
    [(null? xs) 0]
    [(null? (cdr xs)) (car xs)] 
    [(andmap (curry = (car xs)) (cdr xs))  
     (* (length xs) (cdr xs))] 
    [else (apply + xs)]))

(define-symbolic xs integer? #:length 4)

(assume (positive? (sum xs)))

(verify (assert (ormap positive? xs)))