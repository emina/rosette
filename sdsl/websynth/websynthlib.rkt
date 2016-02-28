#lang rosette

(require "dom.rkt")
(provide (except-out (all-defined-out) tags))

(define-syntax-rule (define-tags ts)
  (tags ts))

(define tag? integer?)

(define tags (make-parameter (cons (hash "" 0) (vector ""))
                             (lambda (vs)
                               (cons
                                (for/hash ([(s i) (in-indexed (cons "" vs))])
                                 (values s i))
                                (list->vector (cons "" vs))))))

(define (tag str) (hash-ref (car (tags)) str))

(define (label i) (vector-ref (cdr (tags)) i))

; Maximum depth of the DOM, so we know how many variables
; to allocate. (Writen by Emina Torlak)
(define (depth dom)
  (if (DOMNode? dom)
      (+ 1 (apply max (cons 0 (map depth (DOMNode-content dom)))))
      0))

(define (size dom )
  (if (DOMNode? dom)
      (+ 1 (apply + (map size (DOMNode-content dom))))
      0))

; Checker function that returns true iff a prefix of the 
; given path connects the source node to the sink node. (Writen by Emina Torlak)
(define (path? path source sink)
  (or (and (equal? source sink)
           (andmap (lambda (p) (equal? p (tag ""))) path))
      (and (DOMNode? source) 
           (not (null? path))
           (equal? (car path) (tag (DOMNode-tagname source)))
           (ormap (lambda (child) (path? (cdr path) child sink)) (DOMNode-content source)))))

; Convert the final evaluated solution into a zpath string
(define (synthsis_solution->zpath zpath_list)
  ;(string-append "/" (string-join (remove* (list "") (cdr zpath_list)) "/")))
  (string-append "/" (string-join (remove* (list "") zpath_list) "/")))

; Mask function
(define (generate-mask zpath1 zpath2 mask depth)
  (unless (= (length zpath1) 0)
    (assert (eq? (car mask) 
                 (eq? (car zpath1) (car zpath2))))
    (generate-mask (cdr zpath1) (cdr zpath2) (cdr mask) depth)))

; Zip
; Found at http://jeremykun.wordpress.com/2011/10/02/a-taste-of-racket/ in the comments.
(define (zip list . lists)
  (apply map (cons (lambda (x . xs) (cons x xs)) (cons list lists))))
