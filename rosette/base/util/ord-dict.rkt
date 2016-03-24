#lang racket

(provide ord-dict? [rename-out (make-ordered-dictionary ord-dict)
                               (make-immutable-ordered-dictionary immutable-ord-dict)]
         last-key last-value
         first-key first-value
         dict-take dict-drop)
         
(define (last-key dict)
  (last (ord-dict-order dict)))

(define (last-value dict)
  (ord-dict-ref dict (last-key dict)))

(define (first-key dict)
  (first (ord-dict-order dict)))

(define (first-value dict)
  (ord-dict-ref dict (first-key dict)))

(define (dict-take dict pos)
  (sub-dict dict (take (order dict) pos)))

(define (dict-drop dict pos) 
  (sub-dict dict (drop (order dict) pos)))

(define (sub-dict dict sub-order)
  (let ([tbl (table dict)])
    (ord-dict (for/hash ([key sub-order]) (values key (dict-ref tbl key)))
              sub-order)))

(define ord-dict-ref 
  (case-lambda [(dict key) (dict-ref (table dict) key)]
               [(dict key failure-result) (dict-ref (table dict) key failure-result)]))
                                                   
(define (ord-dict-set! dict key value) 
  (unless (dict-has-key? (table dict) key) 
    (set-order! dict (append (order dict) (list key))))
  (dict-set! (table dict) key value))
                                                   
(define (ord-dict-remove! dict key) 
  (when (dict-has-key? (table dict) key) 
    (set-order! dict (remove key (order dict)))
    (dict-remove! (table dict) key)))

(define (ord-dict-count dict) (dict-count (table dict))) 

(define (ord-dict-iterate-first dict)
  (and (not (null? (order dict)))
       (order dict)))

(define (ord-dict-iterate-next dict pos)
  (and (not (null? pos))
       (not (null? (cdr pos)))
       (cdr pos)))

(define (ord-dict-iterate-key dict pos) (car pos))

(define (ord-dict-iterate-value dict pos)
  (dict-ref (table dict) (car pos)))

(struct ord-dict (table [order #:mutable])
  #:property prop:dict
  (vector ord-dict-ref 
          ord-dict-set! #f 
          ord-dict-remove! #f 
          ord-dict-count 
          ord-dict-iterate-first ord-dict-iterate-next 
          ord-dict-iterate-key ord-dict-iterate-value)
  #:property prop:custom-write 
  (lambda (self port mode) 
    (let ([order (order self)]
          [table (table self)])
      (fprintf port "ordered-dict~s" (map (lambda (key) (cons key (dict-ref table key))) order)))))

(struct immutable-ord-dict ord-dict ()
  #:property prop:dict
  (vector ord-dict-ref 
          #f #f 
          #f #f 
          ord-dict-count 
          ord-dict-iterate-first ord-dict-iterate-next 
          ord-dict-iterate-key ord-dict-iterate-value))

(define table ord-dict-table)
(define order ord-dict-order)
(define set-order! set-ord-dict-order!)

(define (make-ordered-dictionary [assocs null])
  (ord-dict (make-hash assocs) (map car assocs)))

(define (make-immutable-ordered-dictionary dict)
  (if (immutable-ord-dict? dict) 
      dict
      (let ([dict-hash (for/hash ([(key value) (in-dict dict)]) (values key value))])
        (if (ord-dict? dict) 
            (immutable-ord-dict dict-hash (order dict))
            (immutable-ord-dict dict-hash (for/list ([key (in-dict-keys dict)]) key))))))
       
       
                   