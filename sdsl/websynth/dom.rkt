#lang racket

(require (only-in html read-html-as-xml) xml net/url net/uri-codec)

(provide 
 read-DOMNode fetch-DOMNode
 DOMNode
 DOMNode?
 DOMNode-tagname 
 DOMNode-attributes
 DOMNode-content
 tags
 xpath-to-tokens
 abs-search
 token-matches?
 apply-mask)

(struct DOMNode (tagname attributes content) #:transparent)

(define (blank? value)
  (and (string? value) (regexp-match? #px"^\\s+$" value)))

(define (xml->DOMNode xml [index (make-hash)])
  (match xml
    [(element _ _ name (list (attribute _ _ key val) ...) content)
     (hash-update! index name add1 -1)
     (DOMNode (format "~a[~a]" name (hash-ref index name))
              '();(map cons key val)
              (let loop ([content content] [index (make-hash)])
                (if (null? content) 
                    '()
                    (match (xml->DOMNode (car content) index)
                      ["" (loop (cdr content) index)]
                      [other (cons other (loop (cdr content) index))]))))]           
    [(pcdata _ _ val) (if (blank? val) "" (string->immutable-string val))]
    [other ""]));(printf "~a\n" other) ""]))

(define (read-DOMNode file)
  ;(printf "~a\n" (first (filter element? (read-html-as-xml (open-input-file file)))))
  (xml->DOMNode 
   (first (filter element? (read-html-as-xml (open-input-file file))))))

; Returns the DOMNode corresponding to the given URL, provided in the form of a string.
(define (fetch-DOMNode ustr)
  (xml->DOMNode
   (first (filter element? (read-html-as-xml (get-pure-port (string->url ustr)))))))

(define (tags dom)
  (define seen (mutable-set))
  (define keys '())
  (let loop ([dom dom])
    (when (DOMNode? dom)
      (define t (DOMNode-tagname dom))
      (unless (set-member? seen t)
        (set-add! seen t)
        (set! keys (cons t keys)))
      (map loop (DOMNode-content dom))))
  (reverse keys))

; examples
(define s1 "/div/ul/li[0]")
(define s2 "ul")
(define s3 "div[class=\"foo\"]")
(define s4 "ul/li")

(define (xpath-to-tokens selector)
  (remove* (list "") (regexp-split #rx"/" selector)))

(define (token-to-index token)
  (cond
    [(regexp-match? #rx"(\\[)([0-9]+)(\\])" token)
      (string->number (list-ref (regexp-match #rx"(\\[)([0-9]+)(\\])" token) 2))]
    [else null]
  )
)

(define (xpath-absolute? tokens)
  (equal? (car tokens) ""))


(define (token-equal? token tag)
  (let ([tokentagonly (regexp-match #rx"([a-zA-Z0-9]*)\\[" token)]
        [tagonly (regexp-match #rx"([a-zA-Z0-9]*)\\[" tag)] )
    (cond [(and (false? tokentagonly) (false? tagonly))
           (equal? token tag)]
          [(and (not (false? tokentagonly)) (false? tagonly))
           (equal? tag (list-ref tokentagonly 1))]
          [(and (false? tokentagonly) (not (false? tagonly)))
           (equal? (list-ref tagonly 1) token)]
          [(and (not (false? tokentagonly)) (not (false? tagonly)))
           (equal? (list-ref tagonly 1) (list-ref tokentagonly 1))]
        )
    ))

(define (token-matches? token dom)
  (if (string? dom)
    #f
    (token-equal? token (DOMNode-tagname dom))))

(define (index-helper siblings token)
  (let ([index (token-to-index token)])
    (if (null? index)
        siblings
        (cond [(> (length siblings) index)  (list (list-ref siblings index))]
              [(= index 0) '()]
              [else (error (string-append "Index out of bounds. Tag: " token "; index: " (number->string index)
                                          "; siblings: " (number->string (length siblings))))]))))

(define (apply-mask zpath mask)
  (let ([newpath '()])
    (for ([i (in-range 0 (- (length mask) 1))])
      (if (list-ref zpath i)
        (if (list-ref mask i)
          (set! newpath (append newpath (list (list-ref zpath i))))
          (set! newpath
              (append newpath (list (list-ref (regexp-match #rx"([a-zA-Z0-9]*)\\[" (list-ref zpath i)) 1))))
        )
        newpath
      )
    )
    newpath
  )
)

(define (abs-search-helper dom xpath)
  (cond
    [(= (length xpath) 0)]
    [(= (length xpath) 1)      
        (if (token-matches? (car xpath) dom)
        dom
        (list))
      ]
    [else 
      (flatten (map (lambda (child) (abs-search-helper child (cdr xpath)))
                      (index-helper 
                        (filter (lambda (child) (token-matches? (cadr xpath) child)) (DOMNode-content dom))
                        (cadr xpath))
                        ))]
  )
)

; Given an absolute (tokenized) xpath, return a list of all matching DOMNodes.
(define (abs-search dom xpath)
  (abs-search-helper dom xpath)
)

(define (print/return str #:val val . rest)
  (apply printf str val rest)
  val)