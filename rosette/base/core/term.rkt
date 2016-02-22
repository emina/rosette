#lang racket

(require racket/syntax (for-syntax racket racket/syntax) "type.rkt" "op.rkt")

(provide
 term-cache
 term?             ; (-> any/c boolean?)
 (rename-out [a-term term] [an-expression expression] [a-constant constant])     ; pattern matching macro
 term-name         ; (-> term? (or/c syntax? false/c))
 term-index        ; (-> term? (or/c false/c number? list?))
 term-op           ; (-> term? (or/c op? false/c))
 term-child        ; (-> term? (listof any/c))  
 term-type         ; (-> term? type?)  
 constant?         ; (-> any/c boolean?)
 angelic?          ; (-> any/c boolean?)
 expression?       ; (case-> (-> any/c boolean?) (-> any/c op? boolean?))
 term<?            ; (-> term? term? boolean?)
 term-origin       ; (-> term? any/c)
 term-track-origin ; (-> term? any/c term?)
 term-property     ; (case-> (-> term? symbol? any/c) (-> term? symbol? any/c term?))
 term-e            ; (-> any/c any/c)
 term->datum       ; (-> any/c any/c)
 term->list        ; (-> any/c any/c)
 clear-terms!      ; (-> void? void?)
 sublist?
 (all-from-out "type.rkt"))

(define angelic?
  (match-lambda [(a-constant (not (? identifier?)) _) #t]
                [_ #f])) 

(define term-op 
  (match-lambda [(an-expression op _ ...) op]
                [_ #f]))

(define term-child 
  (match-lambda [(an-expression _ child ...) child]
                [_ '()]))

(define term-name 
  (match-lambda [(a-constant name _) name]
                [_ #f]))

(define term-index
  (match-lambda [(a-constant (cons _ idx) _) idx]
                [_ #f]))

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
           
(define (make-const id-stx t [index #f])
  (unless (identifier? id-stx)
    (error 'constant "expected a syntactic identifier, given ~s" id-stx))
  (unless (type? t)
    (error 'constant "expected a symbolic type, given ~a" t))
  (make-term constant (if index (cons id-stx index) id-stx)  t))

(define (make-expr op . vs)
  (make-term expression (cons op vs) (op-out-type op vs)))

(define-match-expander a-constant
  (lambda (stx)
    (syntax-case stx ()
      [(_ id-pat type-pat)     #'(constant id-pat type-pat _ _)]))
  (lambda (stx)
    (syntax-case stx ()
      [(_ id-stx type)         #'(make-const id-stx type #f)]     
      [(_ id-stx idx type)     #'(make-const id-stx type idx)])))

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
  (val                ; (or/c identifier? (cons/c identifier? number?) (cons/c op? (non-empty-listof any/c)))
   type               ; type?  
   ord                ; integer?  
   [props #:auto #:mutable]) ; (or/c #f hash?)
  #:auto-value #f
  #:methods gen:typed 
  [(define (get-type v) (term-type v))])

(struct constant term ()
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (fprintf port "#<~a:~a>" (const-e self) (get-type self)))])

(struct expression term ()
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (let ([val (term-val self)])
       (fprintf port "#<~a:~a>" 
                (op-name (car val)) 
                (get-type self))))])
   
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
; The following functions convert symbolic values to plain s-expressions.  They are 
; analogous to  Racket's syntax->datum, synatx-e, and syntax->list for unpacking syntax
; expressions.
#|-----------------------------------------------------------------------------------|#

(define (term->datum val) 
  (convert val const-e op-e (make-hash)))

(define (term-e val)
  (cond [(constant? val) (const-e val)]
        [(expression? val) (expr-e val)]
        [else val]))

(define (term->list val)
  (convert val term-name identity (make-hash)))

(define (convert val convert-var convert-op cache)
  (if (hash-has-key? cache val) 
      (hash-ref cache val)
      (let ([datum
             (cond [(constant? val)   (convert-var val)]
                   [(expression? val) `(,(convert-op (term-op val)) 
                                       ,@(for/list ([e (term-child val)]) 
                                           (convert e convert-var convert-op cache)))]
                   [else  val])])
        (hash-set! cache val datum)
        datum)))

(define (const-e var)
  (let ([n (term-name var)])
    (cond [(list? n) (for/fold ([s (format-symbol "~a" (car n))]) ([r (cdr n)]) (format-symbol "~a$~a" s r))]
          [(pair? n) (format-symbol "~a$~a" (car n) (cdr n))]
          [else (format-symbol "~a" n)])))

(define (op-e op) (op-name op))

(define (expr-e expr)
  `(,(term-op expr) ,@(term-child expr)))
  
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