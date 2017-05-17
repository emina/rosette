#lang racket

(require racket/draw (only-in mrlib/graph find-dot))

(provide automaton->bitmap automaton->dot)

(define (automaton->bitmap a)
  (match (find-dot)
    [#f a]
    [dot 
     (define-values (p p-out p-in p-err)
       (subprocess #f #f #f dot "-Tjpeg"))
     (automaton->dot a p-in)
     (close-output-port p-in)
     (read-bitmap p-out 'jpeg)]))
  
(define (automaton->dot a [port (current-output-port)])
  (fprintf port "digraph fsm {\n")
  (fprintf port "rankdir=LR;\n")
  (fprintf port "margin=0;\n")
  (fprintf port "node[fontsize=22, fontname=\"Gill Sans\", margin=0.03];\n")
  (fprintf port "edge[fontsize=22, fontname=\"Gill Sans\"];\n")
  (for ([ne a])
    (match ne
      [(list n)
       (fprintf port "~a [shape=doublecircle];\n" n)]
      [(list n _ ...)
       (fprintf port "~a [shape=circle];\n" n)]))
  (for ([ne a])
    (match ne
      [(list n lts ...)
       (for ([t (remove-duplicates (map second lts))])
         (define lbl (compact-label t lts))
         (if (list? t)
             (for ([target (cdr t)] #:unless (equal? target 'reject))
               (fprintf port "~a -> ~a [label=\"~a\", style=dotted];\n" n target lbl))
             (unless (equal? t 'reject)
               (fprintf port "~a -> ~a [label=\"~a\"];\n" n t lbl))))]))
  (fprintf port "}\n"))


(define (compact-label t lts)
  (apply string-append
         (add-between
          (for/list ([lt lts] #:when (equal? t (second lt)))
            (symbol->string (first lt)))
          ",")))