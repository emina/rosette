#lang rosette

(require rosette/lib/match)

(provide ⊥ ⊤ 0@⊥ let@ R ?int ?label ?bool value? return? 
         (rename-out [boolean? label?] [|| ∨] [=> ⊑]
                     [value @]
                     [value-int @int]
                     [value-label @label]
                     [return-pc Rpc]
                     [return-n Rn]))
                     
; Low/high labels.
(define ⊥ #f)
(define ⊤ #t)

; Returns a printable representation of a label value.
(define (format-label L)
  (cond [(term? L) L]
        [L '⊤]
        [else '⊥]))

; A value consists of an integer and a label.
(struct value (int label)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (match-let ([(value n Ln) self])
       (fprintf port "~s@~s" n (format-label Ln))))])

; Default (zero) value.
(define 0@⊥ (value 0 ⊥))

; A convenience macro for deconstructing and binding values.
(define-syntax let@
  (syntax-rules ()
    [(_ () body ...) 
     (let () body ...)]
    [(_ ([(V L) expr]) body ...)
     (match expr
       [(value V L) body ...])]
    [(_ ([x expr]) body ...)
     (let ([x expr]) body ...)]
    [(_ (bind0 bind ...) body ...)
     (let@ (bind0) (let@ (bind ...) body ...))]))

; Returns a fresh symbolic integer.
; (-> void? integer?)
(define (?int)
  (define-symbolic* n integer?)
  n)

; Returns a fresh symbolic label.
; (-> void? label?)
(define (?label)
  (define-symbolic* L boolean?)
  L)

; Returns a fresh symbolic boolean.
; (-> void? boolean?)
(define (?bool)
  (define-symbolic* b boolean?)
  b)

; A return address wraps a pc value and a value (0 or 1) 
; indicating whether a call returns a value or not.
(struct return (pc n)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "R(~s, ~s)" (return-pc self) (return-n self)))])

; We'll allow return addresses to be created without providing 
; the number of return values.
(define-match-expander R
  (syntax-rules () [(_ pc-pat n-pat) (return pc-pat n-pat)])
  (syntax-id-rules ()
    [(R pc)   (return pc 0@⊥)]
    [(R pc n) (return pc n)]
    [R        return]))
