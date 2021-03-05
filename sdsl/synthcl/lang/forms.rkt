#lang rosette

(require racket/stxparam racket/splicing
         (for-syntax "types.rkt" "errors.rkt" (only-in racket make-list) (only-in syntax/stx stx-null?))
         "types.rkt" "util.rkt"
         (prefix-in rosette/ (only-in rosette if assert void))
         (only-in rosette/lib/synthax define-simple-grammar [?? @??] choose)
         (only-in "../model/runtime.rkt" address-of malloc)
         (only-in "builtins.rkt" NULL clCreateProgramWithSource))

(provide assert 
         print procedure kernel grammar ?? choose
         sizeof @ : = 
         app-or-ref locally-scoped if-statement for-statement range)

; Simplified sizeof operator that takes as input a real type identifier and returns 
; its real-type-length.
(define-syntax (sizeof stx)
  (syntax-case stx ()
    [(sizeof t)
     (syntax/loc stx (real-type-length t))]))

; Simplified addressof operator that takes as input a real lvalue and returns a 
; pointer object through which this value can be accessed.
(define-syntax (@ stx)
  (syntax-case stx ()
    [(@ lval)
     (quasisyntax/loc stx (address-of lval #,(type-name (type-ref #'lval))))]))

; Assertion.
(define-syntax (assert stx)
  (syntax-case stx ()
    [(assert val) 
     (quasisyntax/loc stx 
       (rosette/assert ((bool) val)))]))

; Splits a list of size k*n into k sublists of size n.  The 
; sublists are returned in a list.  The behavior of this function 
; is unspecified if the length of the list is not a multiple of n.  
(define (split-into xs n)
  (if (null? xs) 
      null
      (let-values ([(left right) (split-at xs n)])
        (cons left (split-into right n)))))

; Variable declaration.  Declared real variables are bound to fresh symbolic constants created
; using define-symbolic*. Declared pointer variables with known length are bound to arrays (pointers)
; of that length, also containing fresh define-symbolic* constants.
(define-syntax (: stx)
  (syntax-case stx ()
    [(: type[len] x ...)
     (let ([t (identifier->type #'type stx)])
       (with-syntax ([type* (type-name (base->pointer-type t))]
                     [base (if (scalar-type? t) #'(type-base type) #'(type-base (type-base type)))]
                     [tlen (real-type-length t)])
         (quasisyntax/loc stx
           (define-values (x ...)
             (values (local [(define-symbolic* x base #:length (* len tlen))
                             (define *x ((type*) (malloc (* len (sizeof type)))))]
                       (for ([i len][v (split-into x tlen)])
                         (pointer-set! *x i (apply type v)))
                       *x) ...)))))]
    [(: type x ...)
     (let ([t (identifier->type #'type stx)])
       (if (real-type? t)
           (with-syntax ([base (if (scalar-type? t) #'(type-base type) #'(type-base (type-base type)))]
                         [tlen (real-type-length t)])
             (quasisyntax/loc stx
                (define-values (x ...)
                  (values (local [(define-symbolic* x base #:length tlen)]
                            (apply type x)) ...))))
           (with-syntax ([(v ...) (make-list (length (syntax->list #'(x ...))) (if (equal? t char*) "" #'NULL))])
             (quasisyntax/loc stx
               (define-values (x ...) (values v ...))))))]))

; Assignment (see Ch. 6.3.o of opencl-1.2 spec).
(define-syntax (= stx)
  (syntax-case stx ()
    [(= lval expr)
     (identifier? #'lval)
     (with-syntax ([t (type-name (type-ref #'lval))])
       (syntax/loc stx
         (set! lval ((t) expr))))]
    [(= [loc sel] expr)
     (pointer-type? (type-ref #'loc))
     (with-syntax ([t (type-name (type-base (type-ref #'loc)))])
       (syntax/loc stx
         (pointer-set! loc sel ((t) expr))))]
    [(= [loc sel] expr)
     (vector-type? (type-ref #'loc))
     (syntax/loc stx
       (set! loc (vector-update loc sel expr)))]))

; Procedure application or vector/pointer dereference.
(define-syntax (app-or-ref stx)
  (syntax-case stx ()
    [(_ loc sel)
     (pointer-type? (type-ref #'loc))
     (syntax/loc stx (pointer-ref loc sel))]
    [(_ loc sel)
     (vector-type? (type-ref #'loc))
     (quasisyntax/loc stx (vector-select loc sel #,(type-name (type-ref stx))))]
    [(_ proc ctxt src)
     (and (identifier? #'proc) (free-label-identifier=? #'proc #'clCreateProgramWithSource))
     (quasisyntax/loc stx (proc ((cl_context) ctxt) src))]
    [(_ proc arg ...)
     (quasisyntax/loc stx
       (proc #,@(for/list ([a (syntax->list #'(arg ...))] [t (function-type-args (type-ref #'proc))])
                  (if (type? t) #`((#,(type-name t)) #,a) a))))]))
     

; The if-statement.
(define-syntax (if-statement stx)
  (syntax-case stx ()
    [(if test {then ...}  {else ...})
     (syntax/loc stx 
       (rosette/if ((bool) test) 
           (let () then ... (rosette/void)) 
           (let () else ... (rosette/void))))]
    [(if test {then ...})
     (syntax/loc stx (if-statement test {then ...} {}))]))

; The range expression.
(define-syntax-parameter range (syntax-rules ()))

; The for-statement.
(define-syntax (for-statement stx)
  (syntax-case stx (: in)          
    [(for [(: type var in rangeExpr) ...] expr ...)
     (quasisyntax/loc stx
       (syntax-parameterize 
        ([range (syntax-rules () [(_ arg (... ...)) (in-range arg (... ...))])])
        (for* ([var rangeExpr] ...)
          expr ...
          (rosette/void))))]))

; Procedure syntax.
(define-syntax (procedure stx)
  (syntax-case stx ()
    [(procedure out (id [type param] ...) expr ...)
     (if (equal? void (identifier->type #'out stx))
         (syntax/loc stx
           (define (id param ...)
             expr ...
             (rosette/void)))
         (syntax/loc stx
           (define (id param ...)
             expr ...)))]))

; Kernel syntax.
(define-syntax (kernel stx)
  (syntax-case stx ()
    [(kernel out (id [type param] ...) expr ...)
     (syntax/loc stx 
       (procedure out (id [type param] ...)
          (set! param ((type) param)) ...
          expr ...))]))

(define-syntax (grammar stx)
  (syntax-case stx ()
    [(grammar out (id [type param] ...) expr)
     (quasisyntax/loc stx
       (define-simple-grammar (id param ...) expr))]))

; Constant syntax.
(define-simple-grammar (?? t) (@?? (type-base t)))

; Syntax for creating a local scope for a sequence of statements. 
(define-syntax (locally-scoped stx)
  (syntax-case stx ()
    [(locally-scoped) (syntax/loc stx (begin))]
    [(locally-scoped expr ...) (syntax/loc stx (let () expr ...))]))

; Printing.
(define-syntax (print stx)
  (syntax-case stx ()
    [(print form ...)
     (quasisyntax/loc stx
       (begin
         #,@(for/list ([f (syntax->list #'(form ...))])
              (let ([t (type-ref f)])
                (if (or (real-type? t) (pointer-type? t) (cl-type? t))
                    (quasisyntax/loc f (write #,f))
                    (quasisyntax/loc f (display #,f)))))))]))







#|
; Writes given values to current-output.
(define (write . vs)
  (for ([v vs]) (racket/write v)))

; Writes given values to current-output, followed by a newline.
(define (writeln . vs)
  (apply write vs)
  (newline))

; Displays given values to current-output.
(define (display . vs)
  (for ([v vs]) (racket/display v)))

; Displays given values to current-output, followed by a newline.
(define (displayln . vs)
  (apply display vs)
  (newline))
|#

