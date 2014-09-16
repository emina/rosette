#lang s-exp rosette

(require (only-in racket/runtime-path define-runtime-path))
(require "../dom.rkt")
(require "../websynth.rkt")
(require "../websynthlib.rkt")

(define-runtime-path html (build-path "." "../html/alanon_arkansas.html"))
(define dom (read-DOMNode html))
(define-tags (tags dom))
(define max_zpath_depth 6)

; Record 0 fields
(define-symbolic r0f0zpath tag? [max_zpath_depth])

(current-log-handler (log-handler #:info any/c))
(current-bitwidth 1)

; Record 0 zpath asserts
(assert (path? r0f0zpath dom "Alexander AFG"))


(define sol (solve #t))

; Record 0 zpaths
; Record 1 zpaths

; Construct final zpaths
(define r0f0zpath_list (map label (evaluate r0f0zpath)))
(define field0_zpath (synthsis_solution->zpath r0f0zpath_list))


(printf "DOM stats:  size = ~a, depth = ~a, tags = ~a\n" (size dom) max_zpath_depth (enum-size tag?))
(zip 
(DOM-Flatten (DOM-XPath dom field0_zpath))

)

(define appnd
  (case-lambda [() '()]
               [(xs) xs]
               [(xs ys) (let loop ([xs xs])
                          (if (null? xs)
                              ys
                              (cons (car xs) (loop (cdr xs)))))]
               [(xs . yss) (let loop ([yss yss] [out xs])
                             (if (null? yss)
                                 out
                                 (loop (cdr yss) (appnd out (car yss)))))]))

(define (zpath-interpret zpath dom)
  (cond [(and (not (DOMNode? dom)) (null? zpath)) (list dom)]
        [(and (DOMNode? dom) (equal? (car zpath) (tag (DOMNode-tagname dom))))
         (define t (map (curry zpath-interpret (cdr zpath)) (DOMNode-content dom)))
         (printf "zpath: ~a, t: ~a, possibilities: ~a\n" (length zpath) t
                 (apply * (map (lambda (x) (if (union? x) (length (union-contents x)) 1 )) t)))
         (apply append t)] ;(map (curry zpath-interpret (cdr zpath)) (DOMNode-content dom)))]
        [else '()]))

(define (leaves dom)
  (cond [(DOMNode? dom)
         (append-map leaves (DOMNode-content dom))] ;(map (curry zpath-interpret (cdr zpath)) (DOMNode-content dom)))]
        [else (list dom)]))