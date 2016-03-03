#lang racket

(require racket/syntax (for-syntax racket racket/syntax) "type.rkt")

(provide
 term-cache clear-terms!
 term? constant? expression? 
 (rename-out [a-term term] [an-expression expression] [a-constant constant]) 
 term-type term<? sublist?
 define-operator op? op-name op-safe op-unsafe 
 (all-from-out "type.rkt"))

#|-----------------------------------------------------------------------------------|#
; Term cache stores terms for the purposes of partial cannonicalization.
; That is, it ensures that no syntactically identical terms are created.
; It also assigns unique IDs (creation timestamps) to terms.  These IDs
; are never reused, and they are used to impose an ordering on the children
; of expressions with commutative operators.
#|-----------------------------------------------------------------------------------|#
(define term-cache (make-parameter (make-hash)))
(define term-count (make-parameter 0)) 

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

#|-----------------------------------------------------------------------------------|#
; The term structure defines a symbolic value, which can be a variable or an expression.
; The val field of a constant is its unique identifier, and it can be anything.  The val
; field of an expression is a list, in which the first argument is always a function.
; That function can be interpreted (that is, an operator), or uninterpreted (that is,
; its interpretation is determined by the solver).
#|-----------------------------------------------------------------------------------|#
(struct term 
  (val                 ; (or/c any/c (cons/c function? (non-empty-listof any/c)))
   type                ; type?  
   ord)                ; integer?  
  #:methods gen:typed 
  [(define (get-type v) (term-type v))]
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (term->string self)))])

(struct constant term ())
(struct expression term ())
   
(define (term<? s1 s2) (< (term-ord s1) (term-ord s2)))

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
      [(_ id-pat type-pat)     #'(constant id-pat type-pat _)]))
  (syntax-id-rules ()
    [(_ id type) (make-const id type)]
    [_ make-const]))

(define-match-expander an-expression
  (lambda (stx)
    (syntax-case stx ()
      [(_ op-pat elts-pat ...) #'(expression (list op-pat elts-pat ...) _ _)]))
  (syntax-id-rules ()
    [(_ op elts ...) (make-expr op elts ...)]
    [_ make-expr]))

(define-match-expander a-term
  (syntax-rules ()
    [(_ val-pat type-pat) (term val-pat type-pat _)]))


; By default, an op application uses the safe (lifted) version of the operation.  
; This version performs type checking on the arguments, and asserts the preconditions, if any, 
; before calling the unsafe version of the operator.  The unsafe version is used 
; internally by Rosette for efficiency.  It assumes that all of its arguments are 
; properly typed and that all preconditions are met.
(struct op 
  (name safe unsafe type)  
  #:property prop:procedure 
  (struct-field-index safe)
  #:methods gen:custom-write
  [(define (write-proc self port mode) (fprintf port "~s" (op-name self)))])

(define (make-op #:unsafe unsafe #:safe [safe unsafe] #:type type #:name [name (object-name unsafe)] )
  (let ([str-name (format "~s" name)]) 
    (op 
     (string->symbol str-name) 
     safe unsafe type)))

(define-syntax-rule (define-operator id arg ...)
  (define id (make-op arg ...)))
    

(define (op-out-type operator args) 
  (match operator
    [(op _ _ _ t)  (apply t args)]))

#|-----------------------------------------------------------------------------------|#
; The following procedures convert symbolic values to strings.
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

(define (any->datum x)
  (if (identifier? x) (syntax->datum x) x))

(define (print-const val cache max-length)
  (display 
   (match val
     [(a-constant (? list? n) _)
      (for/fold ([s (format "~a" (any->datum (car n)))]) 
                                 ([r (cdr n)]) 
        (format "~a$~a" s (any->datum r)))]
     [(a-constant n _) (format "~a" (any->datum n))])))

(define (print-rec val cache max-length)
  (let ([str (if (hash-has-key? cache val)
                 (hash-ref cache val)
                 (let* ([output-str (open-output-string)]
                        [current-pos (file-position (current-output-port))]
                        [output-port (relocate-output-port output-str #f #f current-pos)])
                   (parameterize ([current-output-port output-port])
                     (cond [(constant? val) (print-const val cache max-length)]
                           [(expression? val) (print-expr val cache max-length)] 
                           [else (display val)]))
                   (let ([str (get-output-string output-str)])
                     (hash-set! cache val str)
                     str)))])
    (display str)))

  
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