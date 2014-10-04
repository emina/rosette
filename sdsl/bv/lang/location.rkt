#lang s-exp rosette

(provide (except-out (all-defined-out) set-location!))

; Represents the type of instruction location.
; Values of this type must be totally ordered, 
; and comparable using the operators <?, <=?,
; and =?.  It should also be possible to retrive 
; the ith location value using location-ref.


(define inputs (make-parameter 0))

(define (set-location! loc? loc loc<?)
  (set! location? loc?)
  (set! location  loc)
  (set! location<? loc<?))

;; enum-based

(define-syntax-rule (define-locations size)
  (local [(define-enum location (build-list (+ size (inputs)) values))]
    (set-location! location? location location<?)))

(define location #f)
(define location? #f)
(define location<? #f)

(define (location>? x y) (location<? y x))
(define location=? eq?)
(define location-ordinal ordinal)

(define (location<=? x y)
  (|| (location=? x y) (location<? x y)))

(define (location-bitwidth inputs lib) 0)

#|
;; bitvector-based
(define-syntax-rule (define-locations size) (void))
(define location? number?)
(define location<? <)
(define location>? >)
(define location=? =)
(define location<=? <=)
(define location identity)
(define location-ordinal identity)

(define (location-bitwidth inputs lib)
  (+ (integer-length (+ (length inputs) (length lib))) 1))|#

  


    
#|
(define-signature location^
  (location?         ; type?
   location<?        ; (-> location? location? boolean?)
   location=?        ; (-> location? location? boolean?)
   location<=?       ; (-> location? location? boolean?)
   location-ref))    ; (-> number? location?)

; Returns a location unit that is based on an enumerated typed.
(define (location-enum@ size)
  (unit 
    (import)
    (export location^)
    
    (define-enum location (build-list size values))
    
    (define location=? eq?)
    
    (define (location<=? x y)
      (or (location=? x y) (location<? x y)))
    
    (define (location-ref idx)
      (vector-ref (enum-members location?) idx))))

; Returns a location unit that is based on the bitvector
; (i.e., number?) type.
(define (location-bv@ size)
  (unit
    (import)
    (export location^)
    
    (define location? number?)
    (define location<? <)
    (define location=? =)
    (define location<=? <=)
    (define location-ref identity)))

(provide location^ location-enum@ location-bv@)
|#
