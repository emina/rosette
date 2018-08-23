#lang rosette

; Micro-benchmarks for various list operations.

(require rosette/lib/angelic)
(provide (all-defined-out))


; Construct a symbolic list of up to the given length
(define (symbolic-list len)
  (define lst (build-list len identity))
  (apply choose* (for/list ([i len]) (take lst i))))
