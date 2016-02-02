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
 unsafe-clear-terms!
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

(define (clear-terms!)
  (define tmp (term-cache))
  (term-cache (make-hash))
  (define new-cache (term-cache))
  (for ([(k v) tmp] #:when (constant? v)) 
    (hash-set! new-cache k v)))

(define (unsafe-clear-terms!)
  (hash-clear! (term-cache)))
                
(define-syntax-rule (make-term term-constructor args type) 
  (let ([val args]) 
    (or (hash-ref (term-cache) val #f)
        (hash-ref! (term-cache) val (term-constructor val type (hash-count (term-cache)) #f)))))
   
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
   [props #:mutable]) ; hash?
  #:methods gen:typed 
  [(define (get-type v) (term-type v))]
  #|#:methods gen:equal+hash
  [(define (equal-proc u v rec) (eq? (term-val u) (term-val v)))
   (define (hash-proc u rec)  (eq-hash-code (term-val u)))
   (define (hash2-proc u rec) (eq-hash-code (term-val u)))]|#
  #:methods gen:custom-write
  [(define (write-proc self port mode) 
     (if (expression? self)
         (fprintf port "~.a" (term->datum self))
         (write (const-e self) port)))])

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
    (if (pair? n) 
        (format-symbol "~a$~a" (syntax-e (car n)) (cdr n))
        (format-symbol "~a" (syntax-e n)))))

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