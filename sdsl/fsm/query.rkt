#lang s-exp rosette

(require 
 "automaton.rkt" "lib.rkt"
 rosette/query/debug rosette/lib/tools/render 
 rosette/lib/meta/meta)

(provide define/debug debug-automaton 
         verify-automaton solve-automaton 
         synthesize-automaton matches?)
        
; Returns a symbolic word of length k, drawn from the given alphabet.
(define (word k alphabet)
  (for/list ([i k])
    (define-symbolic* idx number?)
    (list-ref alphabet idx)))

; Returns a symbolic word of length up to k, drawn from the given alphabet.
(define (word* k alphabet)
  (define-symbolic* n number?)
  (take (word k alphabet) n))

(define (word->string w)
  (apply string-append (map symbol->string w)))

(define (matches? regex w)
  (regexp-match? regex (word->string w)))

(define (correct? m regex w)
  (eq? (m w) (matches? regex w)))

(define (verify-automaton m regex [k 4])
  (define w (word* k (alphabet m)))
  (evaluate w (verify (assert (correct? m regex w)))))

(define (solve-automaton m [k 4])
  (define w (word* k (alphabet m)))
  (evaluate w (solve (assert (m w)))))

(define (debug-automaton m regex w)
  (render (debug [boolean?] (assert (correct? m regex w)))))

(define (synthesize-automaton m regex [k 4])
  (define w (word* k (alphabet m)))
  (print-forms (synthesize #:forall w #:guarantee (assert (correct? m regex w)))))
