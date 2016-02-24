#lang racket

(require racket/syntax (for-syntax racket racket/syntax) "type.rkt" "op.rkt")

(provide
 term-cache
 term?             ; (-> any/c boolean?)
 (rename-out [a-term term] [an-expression expression] [a-constant constant])     ; pattern matching macro
 term-type         ; (-> term? type?)  
 constant?         ; (-> any/c boolean?)
 expression?       ; (case-> (-> any/c boolean?) (-> any/c op? boolean?))
 term<?            ; (-> term? term? boolean?)
 term-origin       ; (-> term? any/c)
 term-track-origin ; (-> term? any/c term?)
 term-property     ; (case-> (-> term? symbol? any/c) (-> term? symbol? any/c term?))
 clear-terms!      ; (-> void? void?)
 sublist?
 guarded 
 guarded-test 
 guarded-value
 (all-from-out "type.rkt"))

(define term-cache (make-parameter (make-hash)))
(define term-count (make-parameter 0)) ; term ids will increase forever regardless of cache clearing

; Clears the entire term-cache if invoked with #f (default), or 
; it clears all terms reachable from the given set of leaf terms.
(define (clear-terms! [terms #f])
  (if (false? terms)
      (hash-clear! (term-cache))
      (let ([cache (term-cache)]
            [evicted (list->mutable-set terms)])
        (for ([t terms])
          (hash-remove! cache (term-val t)))
        (let loop ()
          (define delta  
            (for/list ([(k t) cache] #:when (and (list? k) (for/or ([c k]) (set-member? evicted c))))
              t))
          (unless (null? delta)
            (for ([t delta])
              (hash-remove! cache (term-val t))
              (set-add! evicted t))
            (loop))))))


(define-syntax-rule (make-term term-constructor args type) 
  (let ([val args]) 
    (or (hash-ref (term-cache) val #f)
        (let ([ord (term-count)]) 
          (term-count (add1 ord))
          (hash-ref! (term-cache) val (term-constructor val type ord))))))
           
(define (make-const id t)
  (unless (type? t)
    (error 'constant "expected a symbolic type, given ~a" t))
  (make-term constant id t))

(define (make-expr op . vs)
  (make-term expression (cons op vs) (op-out-type op vs)))

(define-match-expander a-constant
  (lambda (stx)
    (syntax-case stx ()
      [(_ id-pat type-pat)     #'(constant id-pat type-pat _ _)]))
  (syntax-id-rules ()
    [(_ id type) (make-const id type)]
    [_ make-const]))

(define-match-expander an-expression
  (lambda (stx)
    (syntax-case stx ()
      [(_ op-pat elts-pat ...) #'(expression (list op-pat elts-pat ...) _ _ _)]))
  (syntax-id-rules ()
    [(_ op elts ...) (make-expr op elts ...)]
    [_ make-expr]))

(define-match-expander a-term
  (syntax-rules ()
    [(_ val-pat type-pat) (term val-pat type-pat _ _)]))


#|-----------------------------------------------------------------------------------|#
; The term structure defines a symbolic value, which can be a variable or an expression.
; Symbolic values can also be annotated with additional information that indicates, 
; for example, where in the code they came from.
#|-----------------------------------------------------------------------------------|#
(struct term 
  (val                ; (or/c any/c (cons/c op? (non-empty-listof any/c)))
   type               ; type?  
   ord                ; integer?  
   [props #:auto #:mutable]) ; (or/c #f hash?)
  #:auto-value #f
  #:methods gen:typed 
  [(define (get-type v) (term-type v))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (term->string self)))])

(struct constant term ())
(struct expression term ())
   
(define (term<? s1 s2) (< (term-ord s1) (term-ord s2)))

(define term-property
  (case-lambda 
    ([v prop] 
     (and (term? v)
          (let ([props (term-props v)])
            (and props (hash-ref props prop #f)))))
    ([v prop prop-val]
     (and prop-val (term? v) (set-term-props! v (hash-set (or (term-props v) (hash)) prop prop-val)))
     v)))
         
(define (term-origin v) (term-property v 'origin))

(define (term-track-origin v origin) (term-property v 'origin origin))

#|-----------------------------------------------------------------------------------|#
; A guarded value is a typed value consisting a boolean? condition (a test) term and 
; a value term.  The type of a guarded value is the same as the type of its value field. 
; The term structure defines a symbolic value, which can be a variable or an expression.
; Symbolic values can also be annotated with additional information that indicates, 
; for example, where in the code they came from.
#|-----------------------------------------------------------------------------------|#
(struct guarded (test value) 
  #:transparent
  #:methods gen:typed
  [(define (get-type self) (type-of (guarded-value self)))])

#|-----------------------------------------------------------------------------------|#
; The following functions convert symbolic values to strings or plain s-expressions.
#|-----------------------------------------------------------------------------------|#

(define (term->string val [max-length (error-print-width)])
  (let ([output-str (open-output-string)])
    (parameterize ([current-output-port output-str])
      (print-rec val (make-hash) max-length))
    (get-output-string output-str)))

(define (print-expr val cache max-length)
  (match-let ([o (current-output-port)]
              [(an-expression op child ...) val])
    (display "(")
    (display (op-name op))
    (display " ")
    (let ([n (for/sum ([(e i) (in-indexed child)]
                       #:break (>= (file-position o) max-length))
               (print-rec e cache max-length)
               (unless (= i (sub1 (length child)))
                 (display " "))
               1)])
      (when (< n (length child))
        (display "...")))
    (display ")")))

(define (print-guarded val cache max-length)
  (match-let ([o (current-output-port)]
              [(guarded test value) val])
    (printf "[")
    (print-rec test cache max-length)
    (display " ")
    (if (< (file-position o) max-length)
        (print-rec value cache max-length)
        (display "..."))
    (display "]")))
  
(define (print-rec val cache max-length)
  (let ([str (if (hash-has-key? cache val)
                 (hash-ref cache val)
                 (let* ([output-str (open-output-string)]
                        [current-pos (file-position (current-output-port))]
                        [output-port (relocate-output-port output-str #f #f current-pos)])
                   (parameterize ([current-output-port output-port])
                     (cond [(constant? val) (display (const-e val))]
                           [(expression? val) (print-expr val cache max-length)]
                           [(guarded? val) (print-guarded val cache max-length)]
                           [else (display val)]))
                   (let ([str (get-output-string output-str)])
                     (hash-set! cache val str)
                     str)))])
    (display str)))

(define (maybe-identifier x)
  (if (identifier? x) (syntax->datum x) x))

(define (const-e const)
  (match const
    [(a-constant n _)
     (cond [(list? n) (for/fold ([s (format "~a" (maybe-identifier (car n)))]) 
                                ([r (cdr n)]) 
                        (format "~a$~a" s (maybe-identifier r)))]
           [else (format "~a" (maybe-identifier n))])]))
  
#|-----------------------------------------------------------------------------------|#
; Utilities for working with terms.
#|-----------------------------------------------------------------------------------|#
; Returns #t if ys contains all elements of xs, in the order 
; in which they occur in xs. Otherwise returns #f.
(define (sublist? xs ys)
  (and (<= (length xs) (length ys))
       (match xs
         [(list) #t]
         [(list x xs ...)
          (match ys 
            [(list _ ... (== x) ys ...) (sublist? xs ys)]
            [_ #f])])))