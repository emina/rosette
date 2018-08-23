#lang racket

(require racket/syntax (for-syntax racket racket/syntax) racket/generic
         "type.rkt" "reporter.rkt")

(provide
 term-cache clear-terms!
 term? constant? expression? 
 (rename-out [a-term term] [an-expression expression] [a-constant constant]) 
 term-type term<? sublist? @app
 define-operator operator? operator-unsafe
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
  #:property prop:custom-print-quotable 'never
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (term->string self)))])

(struct constant term ())

(struct λconstant constant (procedure)
  #:property prop:procedure [struct-field-index procedure])

(struct expression term ())
   
(define (term<? s1 s2) (< (term-ord s1) (term-ord s2)))

(define-syntax-rule (make-term term-constructor args type rest ...) 
  (let ([val args])
    (or (hash-ref (term-cache) val #f)
        (let* ([ord (term-count)]
               [out (term-constructor val type ord rest ...)])
          (term-count (add1 ord))
          ((current-reporter) 'new-term out)
          (hash-set! (term-cache) val out)
          out))))
           
(define (make-const id t) 
  (unless (and (type? t) (solvable? t))
    (error 'constant "expected a solvable type, given ~a" t))
  (if (type-applicable? t)      
      (letrec ([c (make-term λconstant id t 
                             (procedure-reduce-arity
                              (lambda args (apply @app c args))
                              (length (solvable-domain t))))])
        c)
      (make-term constant id t)))

(define (make-expr op . vs)
  (define ran (operator-range op))
  (make-term expression (cons op vs) (apply ran vs)))

(define-match-expander a-constant
  (lambda (stx)
    (syntax-case stx ()
      [(_ id-pat type-pat) #'(constant id-pat type-pat _)]))
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

#|-----------------------------------------------------------------------------------|#
; An operator is a special kind of procedure that can appear as the first element of
; a term expression.  The range of an operator is given by a procedure that takes as input
; as many arguments as the operator and that returns the type? of the resulting value. 
;
; All operators have a 'safe' and 'unsafe' version.  The 'safe' version checks that
; the operator's arguments are in its domain (by emitting appropriate assertions),
; while the 'unsafe' version assumes that all of its arguments are properly typed and
; that all of its preconditions are met.  Client code sees only the 'safe' version.
; The 'unsafe' variant is used internally by Rosette for efficiency.
#|-----------------------------------------------------------------------------------|#
(struct operator (identifier range safe unsafe)
  #:property prop:procedure 
  (struct-field-index safe)
  #:property prop:object-name
  (struct-field-index identifier)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (id->string (operator-identifier self))))])

(define (make-operator #:unsafe unsafe #:safe [safe unsafe]
                       #:range ran #:identifier [name (object-name unsafe)] )
  (operator (string->symbol (~s name)) ran safe unsafe))

(define-syntax-rule (define-operator id arg ...)
  (define id (make-operator arg ...)))

(define-operator @app
  #:identifier 'app
  #:range  (lambda (f . args)
             (solvable-range (get-type f)))
  #:unsafe (lambda (f . args)
             (if (constant? f)
                 (apply make-expr @app f args)
                 (apply f args)))
  #:safe   (lambda (f . args)
             (if (constant? f)
                 (let ([name (string->symbol (~a f))])
                   (apply make-expr @app f
                          (for/list ([a args][t (solvable-domain (get-type f))])
                            (type-cast t a name))))
                 (apply f args))))
                             
#|-----------------------------------------------------------------------------------|#
; The following procedures convert symbolic values to strings.
#|-----------------------------------------------------------------------------------|#

(define (term->string val [max-length (error-print-width)])
  (let ([output-str (open-output-string)])
    (parameterize ([current-output-port output-str])
      (print-rec val (make-hash) max-length))
    (get-output-string output-str)))

(define (any->datum x)
  (if (identifier? x) (syntax->datum x) x))

(define (id->string val)
  (if (list? val)
       (for/fold ([s (format "~a" (any->datum (car val)))]) 
                 ([r (cdr val)]) 
         (format "~a$~a" s (any->datum r)))
       (format "~a" (any->datum val))))

(define (print-const val cache max-length)
  (display (id->string (term-val val))))

(define (print-expr val cache max-length)
  (match-let ([o (current-output-port)]
              [(an-expression op child ...) val])
    (display "(")
    (display (id->string (operator-identifier op)))
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
