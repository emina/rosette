#lang racket

(require (only-in racket/syntax format-id))

(provide syntax-identifier generate-identifiers)

(define (syntax-identifier stx)
  (match stx
    [(? identifier?) stx]
    [(and (? syntax?) (app syntax->list (list (? identifier? id) _ ...))) id]
    [_ (raise-argument-error 'syntax-identifier "an identifier? or a syntax list that starts with an identifier" stx)]))


(define counter 0)

(define (append-number s)
  (set! counter (add1 counter))
  (string->symbol (format "~a~s" s counter)))

(define (generate-identifiers stx #:base [name-base 'sym] #:source [src #f])
  (let* ([stxs (cond [(list? stx) stx]
                     [(syntax? stx) (or (syntax->list stx) stx)]
                     [(and (integer? stx) (positive? stx)) (for/list ([i (in-range 0 stx)]) name-base)]
                     [else (raise-argument-error 'generate-identifiers "list?, syntax? or a positive? integer?" stx)])])
    (for/list ([expr stxs])
      ((make-syntax-introducer)
       (cond [(syntax? expr) (format-id expr "~a" 
                                        (append-number (if (identifier? expr) (syntax-e expr) name-base)) 
                                        #:source (if (syntax-position expr) expr src))]
             [(symbol? expr) (format-id #f   "~a" (append-number expr) #:source src)]
             [else           (format-id #f   "~a" (append-number name-base) #:source src)])))))
