#lang rosette

(require "types.rkt")

(provide source-of parse-selector check-selector)



; Formats and returns source information for the given syntax.
(define (source-of stx)
  (format "~a:~a:~a" (syntax-source stx) (syntax-line stx) (syntax-column stx)))

; Parses the given idx syntax as a vector selector identifier
; (as defined in Ch. 6.1.7 of opencl-1.2 specification), allowing 
; duplicates if the allow-duplicates? flag is true.  If the given 
; identifier cannot parsed as a vector selector, throws a syntax error 
; using the given context syntax for error localization.
(define (parse-selector allow-duplicates? idx context)
  (match (symbol->string (syntax->datum idx))
    [(or (pregexp #px"^s([a-fA-F\\d]+)$" (list _ pos))
         (pregexp #px"^([xyzw]+)$" (list _ pos)))
     (case (string-length pos)
       [(1 2 3 4 8 16) 
        (let ([selector (for/list ([char (in-string pos)])
                          (character->selector char))])
          (unless (or allow-duplicates? 
                      (= (length (remove-duplicates selector)) (length selector)))
            (raise-syntax-error #f "component selector contains duplicates" context idx))
          selector)]
       [else (raise-syntax-error #f "wrong number of component selectors" context idx)])]
    [_ (raise-syntax-error #f "invalid component selector" context idx)]))
             
(define (character->selector char)
  (case char
    [(#\0 #\x) 0]
    [(#\1 #\y) 1]
    [(#\2 #\z) 2]
    [(#\3 #\w) 3]
    [else (string->number (string char) 16)]))

; Checks that the given list of selector indices is valid
; for the specified vector type.  If not, throws a syntax 
; error using the given syntaxes as context.
(define (check-selector selector vector-type expr [subexpr #f])
  (define len (real-type-length vector-type))
  (for ([idx selector])
    (unless (< idx len)
      (raise-syntax-error #f "component selector out of bounds" expr subexpr))))
  
                        

