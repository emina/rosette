#lang racket

(provide (rename-out [syntax->location location])
         location? location-source location-start location-end
         location-contains? read-module)

(struct location (source start end) #:transparent)

(define (syntax->location stx) 
  (let ([pos (syntax-position stx)]
        [span (syntax-span stx)])
    (and pos span (location (syntax-source stx) pos (+ pos span)))))

(define (location-contains? outer inner)
  (and (equal? (location-source outer) (location-source inner))
       (<= (location-start outer) (location-start inner))
       (>= (location-end outer) (location-end inner))))

(define (read-module path)
  (parameterize ([read-accept-reader #t]
                 [port-count-lines-enabled #t]) 
    (read-syntax path (open-input-file path))))
