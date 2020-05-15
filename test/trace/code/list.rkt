#lang rosette

(define-symbolic b boolean?)
(map (thunk* (error 'bad)) (if b '() '(1)))
