#lang racket

(require (for-syntax racket/syntax) 
         (for-syntax racket/stxparam syntax/template)
         racket/stxparam racket/stxparam-exptime)

(provide syntax/source)


(define-for-syntax (get-source stx)
  (list (syntax-source stx) 
        (syntax-line stx)
        (syntax-column stx) 
        (syntax-position stx) 
        (syntax-span stx)))
 
(define (add-source v stx datum) 
  ;(printf "add ~a ~a ~a\n" v stx datum) 
  (datum->syntax stx datum v stx stx))

(define (add-leaf-source v stx) 
 ;(printf "add-leaf ~a ~a\n" v stx) 
 (datum->syntax stx (syntax-e stx) v stx stx))

(define-syntax (syntax/source stx)
  (syntax-case stx ()
    [(_ tmpl) (transform-template #'tmpl
                                  #:save get-source
                                  #:restore-stx #'add-source
                                  ;#:leaf-save-proc get-source
                                  #:leaf-restore-stx #'add-leaf-source)]))