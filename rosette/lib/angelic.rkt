#lang rosette

(provide choose*)

; Implements the angelic choice operator.
(define choose*
  (case-lambda 
    [()  (error 'choose* "expected at least one argument")]
    [(x) x]
    [(x y) (define-symbolic* x? boolean?)
           (if x? x y)]
    [xs    (define-symbolic* xi? boolean? #:length (sub1 (length xs)))
           (let loop ([xi? xi?][xs xs])
             (if (or (null? xi?) (car xi?)) 
                 (car xs)
                 (loop (cdr xi?) (cdr xs))))]))
