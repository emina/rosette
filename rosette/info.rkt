#lang info

;; Runs the code in `private/install.rkt` before installing this collection.


(define pre-install-collection "private/install.rkt")
(define compile-omit-files '("private/install.rkt"))
