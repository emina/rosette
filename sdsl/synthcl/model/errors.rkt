#lang rosette

(provide raise-conversion-error raise-pointer-cast-error raise-context-error)

(define (raise-conversion-error v type)
  (error 'implicit-conversion "cannot convert ~a to ~a" v type))

(define (raise-pointer-cast-error v pointer-type-base)
  (error 'pointer-cast "cannot cast ~a to ~a*" v pointer-type-base))

(define (raise-context-error caller type-of-caller type-of-expected-value given)
  (raise-argument-error 
   caller (format "~a with the same context as the ~a" 
                  type-of-expected-value type-of-caller) 
   given))
