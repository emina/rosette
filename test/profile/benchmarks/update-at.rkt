#lang rosette

(require "list.rkt")
(provide (all-defined-out))


(define (update-at lst pos val)
  (match lst
    [(list) lst]
    [(list x xs ...)
     (if (= pos 0)
         (cons val xs)
         (cons x (update-at xs (- pos 1) val)))]))


; Simple test for update-at
(define (test-update-at lst)
  (define-symbolic* idx integer?)
  (update-at lst idx -1)
  (void))

(define lst (build-list 50 identity))


(time (test-update-at lst))
