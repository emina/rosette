#lang racket

(require (for-syntax racket/syntax) 
         (for-syntax racket/stxparam syntax/template)
         racket/stxparam racket/stxparam-exptime)

(provide (for-syntax save-properties) 
         restore-properties
         syntax/source)

; These parameters store procedures that are used 
; to save and restore desired syntax properties, 
; in addition to source information, for the purpose 
; of synthesis.

; The save-properties parameter stores a procedure that 
; accepts a syntax object and a plain Racket value 
; representing the properties and values to save.  
; By default, save-properties returns #f.
(define-for-syntax save-properties
  (make-parameter
   (lambda (stx) #f)))

; The restore-properties parameter stores a procedure that 
; accepts a syntax object and a plain Racket value representing
; saved properties, producing a copy of the syntax object, optionally
; annotated with the saved properties.  By default, save-properties 
; simply returns the syntax object.
(define restore-properties
  (make-parameter
   (lambda (stx props) stx)))

(define-for-syntax (get-source stx)
  (list ((save-properties) stx)
        (syntax-source stx) 
        (syntax-line stx)
        (syntax-column stx) 
        (syntax-position stx) 
        (syntax-span stx)))
 
(define (add-source v stx datum)
  ((restore-properties) (datum->syntax stx datum (cdr v) stx stx) (car v)))

(define (add-leaf-source v stx)
  ((restore-properties) (datum->syntax stx (syntax-e stx) (cdr v) stx stx) (car v)))

(define-syntax (syntax/source stx)
  (syntax-case stx ()
    [(_ tmpl) 
     (transform-template #'tmpl
                         #:save get-source
                         #:restore-stx #'add-source
                         #:pvar-save get-source
                         #:pvar-restore-stx #'add-leaf-source
                         #:leaf-restore-stx #'add-leaf-source)]))