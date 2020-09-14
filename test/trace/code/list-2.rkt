#lang rosette

(define (map f xs)
  (cond
    [(empty? xs) '()]
    [else (cons (f (first xs)) (map f (rest xs)))]))

(define-symbolic b boolean?)
(map (thunk* (error 'bad)) (if b '() '(1)))
