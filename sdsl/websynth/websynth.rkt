#lang racket
(require racket/pretty)
(require "dom.rkt")

(provide 
 DOM-XPath
 DOM-Restriction
 DOM-Filter
 DOM-Tranform
 DOM-Flatten
 )

; Return subsets of the dom in a list based on an XPath like selector
(define (DOM-XPath dom selector)
  (abs-search dom (xpath-to-tokens selector)))

; Redefine the selected nodes as the contents of a new root [document] node.
(define (DOM-Restriction dom selector)
  (DOMNode "[document]" '() (DOM-XPath dom selector)))

; Given a DOM with a subtree, Text or a list of the previous two, flatten them to text
(define (DOM-Flatten dom)
  (cond
    [(string? dom) dom ]
    [(DOMNode? dom) (string-join (map DOM-Flatten (DOMNode-content dom)) " ")]
    [(list? dom) (map DOM-Flatten dom)]))

; Removes matching DOMNodes from the dom
(define (DOM-Filter dom selector)
  dom)

; Transforms the DOM to an XPath compatible form (i.e. 3 rows per record --> 1 row per record)
;  FIXME: need to define transform language elements.
(define (DOM-Tranform dom selector transform)
  dom)

