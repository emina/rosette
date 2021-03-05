#lang rosette

(require (for-template (only-in "forms.rkt" app-or-ref) 
                       (only-in racket quote #%app #%datum) 
                       "types.rkt")
         syntax/stx syntax/id-table
         (prefix-in rosette/ (only-in rosette  = <= >= ))
         "env.rkt" "util.rkt" "errors.rkt" "types.rkt" 
         "operators.rkt" "forms.rkt" "queries.rkt")

(provide typecheck typecheck-module)

; This module implements a simple typechecker for programs 
; in the OpenCL DSL.  The typecheck works directly on the 
; syntax object representation of a program.  The typechecker
; takes as input a program fragment and returns a copy of the 
; fragment with all of its subforms nnotated with their types.  
; The types can be retrieved from a typecheck expression using 
; the type-ref function.

; Typechecks the contents of a module, and returns two values: 
; the typecheck forms (with attached type annotations) and the 
; dictionary mapping all module-level identifiers to their types.
(define (typecheck-module forms)
  (parameterize ([current-env (env)])
    (typecheck-signatures forms) 
    (values (map typecheck forms) (car (current-env)))))

; Adds the signatures of all procedures and kernels in the given list to 
; the current environment.
(define (typecheck-signatures forms)
  (for ([stx forms])
    (syntax-case stx ()
      [(proc out (id [type param] ...) expr ...)
       (and (identifier? #'proc)
            (or (free-label-identifier=? #'proc #'procedure)
                (free-label-identifier=? #'proc #'kernel)
                (free-label-identifier=? #'proc #'grammar)))
       (let ([out-type (identifier->type #'out stx)]
             [arg-types (map (curryr identifier->type stx) (syntax->list #'(type ...)))])
         (when (free-label-identifier=? #'proc #'kernel) 
           (check-no-conversion out-type void stx #'out))
         (bind #'id (function-type arg-types out-type) stx))]
      [_ #f])))

; Typechecks the given program fragment in the current environment
; and returns the result.
(define (typecheck stx)
  (or (and (type-ref stx) stx)
      (cond [(identifier? stx)     (lookup stx)]
            [(not (stx-list? stx)) (typecheck-datum stx)]
            [else                  (typecheck-form stx)])))

; Typechecks a datum.
(define (typecheck-datum stx)
  (let* ([v (syntax->datum stx)]
         [t (or (real-type-of v) (and (string? v) char*))])
    (unless t (raise-syntax-error #f "unrecognized literal value" stx))
    (type-set stx t)))

; Typechecks a declaration of one or more variables.
(define (typecheck-declaration stx)
  (syntax-case stx ()
    [(: type[len] x y ...)
     (and (identifier? #':) (equal? (syntax->datum #':) ':))
     (let* ([vars (syntax->list #'(x y ...))]
            [t (identifier->type #'type stx)]
            [t (if (real-type? t) 
                   (base->pointer-type t)
                   (raise-bad-type-error "real type" t stx #'type))]
            [sz (typecheck #'len)])
       (check-no-conversion (type-ref sz) int stx #'len)
       (for ([var vars]) 
         (unless (program-identifier? var)
           (raise-syntax-error #f "not a valid identifier" stx var))
         (bind var t stx))                              
       (type-set 
        (quasisyntax/loc stx (: type[#,sz] #,@(map lookup vars))) 
        void))] 
    [(: type x y ...)
     (and (identifier? #':) (equal? (syntax->datum #':) ':))
     (let ([vars (syntax->list #'(x y ...))]
           [t (identifier->type #'type stx)])
       (when (equal? t void)
         (raise-syntax-error #f "not a valid type" stx #'type))
       (for ([var vars]) 
         (unless (program-identifier? var)
           (raise-syntax-error #f "not a valid identifier" stx var))
         (bind var t stx))                              
       (type-set 
        (quasisyntax/loc stx (: type #,@(map lookup vars))) 
        void))]
    [_ (raise-bad-form-error "(: type [len] x y ...) or (: type x y ...)" stx)]))

(define (program-identifier? id)
  (and (identifier? id)
       (regexp-match? #px"^[a-zA-Z_]\\w*$" (symbol->string (syntax->datum id)))))

; Typechecks an assertion.
(define (typecheck-assertion stx)
  (syntax-case stx ()
    [(assert val)
     (let ([typed-val (typecheck #'val)])
       (check-implicit-conversion (type-ref typed-val) bool stx)
       (type-set (quasisyntax/loc stx (assert #,typed-val)) void))]
    [_ (raise-bad-form-error "(assert boolExpr)" stx)]))

; Typechecks an assignment operation (see Ch. 6.3.o of opencl-1.2 specification).
(define (typecheck-assignment stx)
  (syntax-case stx ()
    [(op lval expr)
     (identifier? #'lval)
     (let ([tl (typecheck #'lval)]
           [te (typecheck #'expr)])
       (check-implicit-conversion (type-ref te) (type-ref tl) stx)
       (type-set (quasisyntax/loc stx (op #,tl #,te)) void))]
    [(op [loc sel] expr)
     (let* ([tl (typecheck #'loc)]
            [t (type-ref tl)])
       (cond
         [(or (equal? t cl_mem) (equal? t void*))
          (raise-syntax-error #f "cannot dereference a void* pointer" stx #'loc)]
         [(pointer-type? t)
          (let ([idx (typecheck #'sel)]
                [te  (typecheck #'expr)])
            (check-no-conversion (type-ref idx) int stx #'sel)
            (check-implicit-conversion (type-ref te) (type-base t) stx #'expr)
            (type-set (quasisyntax/loc stx (op [#,tl #,idx] #,te)) void))]
         [(vector-type? t)
          (unless (identifier? #'loc) (raise-syntax-error #f "not an lvalue" stx #'loc))
          (let* ([selector (parse-selector #f #'sel stx)]
                 [te (typecheck #'expr)]
                 [xt (type-ref te)])
            (check-selector selector t stx #'sel)
            (unless (and (real-type? xt) 
                         (eq? (length selector) (real-type-length xt))
                         (if (scalar-type? xt) 
                             (equal? xt (type-base t)) 
                             (equal? (type-base xt) (type-base t))))   
              (raise-bad-type-error (base->real-type (type-base t) (length selector)) xt stx #'expr))
            (type-set (quasisyntax/loc stx (op [#,tl '#,selector] #,te)) void))]
         [else (raise-bad-type-error "vector or pointer" t stx #'loc)]))]
    [_ (raise-bad-form-error (format "(~a lvalue expr)" (syntax->datum #'op)) stx)]))

; Typechecks an application or a reference operation
(define (typecheck-app-or-ref stx)
  (syntax-case stx ()
    [(proc arg ...)
     (let* ([args (syntax->list #'(arg ...))]
            [typed-proc (typecheck #'proc)]
            [t (type-ref typed-proc)])
       (cond
         [(function-type? t)
          (let ([typed-args (check-function-args t args stx)])
            (type-set 
             (quasisyntax/loc stx (#,typed-proc #,@typed-args));(app-or-ref #,typed-proc #,@typed-args))
             (function-type-result t)))]
         [(or (equal? t cl_mem) (or (equal? t void*)))
          (raise-syntax-error #f "cannot dereference a void* pointer" stx #'proc)]
         [(and (pointer-type? t) (rosette/= 1 (length args)))
          (let ([typed-arg (typecheck (car args))])
            (check-no-conversion (type-ref typed-arg) int stx (car args))
            (type-set (quasisyntax/loc stx  (app-or-ref #,typed-proc #,typed-arg)) (type-base t)))]
         [(and (vector-type? t) (rosette/= 1 (length args)))
          (let ([selector (parse-selector #t (car args) stx)])
            (check-selector selector t stx (car args))  
            (type-set 
             (quasisyntax/loc stx (app-or-ref #,typed-proc '#,selector))
             (base->real-type (type-base t) (length selector))))]
         [else
          (raise-bad-form-error "procedure application, or vector/pointer access" stx)]))]))
                 
(define (check-function-args t args stx)
  (let* ([arg-types (function-type-args t)]
         [arg-types (if (list? arg-types) arg-types (make-list (length args) arg-types))])
    (unless (rosette/= (length arg-types) (length args))
      (raise-bad-form-error (format "~a arguments" (length arg-types)) stx))
    (for/list ([arg-type arg-types] [arg (map typecheck args)])
      (check-implicit-conversion (type-ref arg) arg-type stx arg)
      arg)))

; Typechecks the locally-scoped form.
(define (typecheck-locally-scoped stx)
  (syntax-case stx ()
    [(locally-scoped) (type-set stx void)]
    [(locally-scoped form ...)
     (let* ([typed-forms (parameterize ([current-env (env)])
                           (map typecheck (syntax->list #'(form ...))))]
            [t (type-ref (last typed-forms))])
       (type-set (quasisyntax/loc stx (locally-scoped #,@typed-forms)) t))]))

; Typechecks a query declaration, which is either a range declaration 
; or a regular one-variable declaration.
(define (typecheck-query-declaration stx)
  (syntax-case stx ()
    [(: type [expr] var)        (typecheck-declaration stx)]
    [(: type var)               (typecheck-declaration stx)]
    [(: type var in (expr ...)) (typecheck-range-declaration stx)]
    [_ (raise-bad-form-error "(: type[expr] var) or (: int var in (range [start] end [step])" stx)]))
    
; Typechecks the verify or synthesis query statement.
(define (typecheck-query stx)
  (syntax-case stx ()
    [(query #:forall [decl ...] #:bitwidth bw #:grammar-depth depth #:ensure form)
     (parameterize ([current-env (env)])
       (with-syntax ([(typed-decl ...) (map typecheck-query-declaration (syntax->list #'(decl ...)))]
                     [typed-form       (typecheck #'form)]
                     [typed-bw         (typecheck #'bw)]
                     [typed-depth      (typecheck #'depth)])
         (check-no-conversion (type-ref #'typed-bw) int #'typed-bw stx)
         (check-no-conversion (type-ref #'typed-depth) int #'typed-depth stx)
         (type-set (syntax/loc stx (query #:forall [typed-decl ...]
                                          #:bitwidth typed-bw
                                          #:grammar-depth typed-depth
                                          #:ensure typed-form))
                   void)))]
    [(query #:forall [decl ...] #:bitwidth bw #:ensure form)
     (parameterize ([current-env (env)])
       (with-syntax ([(typed-decl ...) (map typecheck-query-declaration (syntax->list #'(decl ...)))]
                     [typed-form       (typecheck #'form)]
                     [typed-bw         (typecheck #'bw)])
         (check-no-conversion (type-ref #'typed-bw) int #'typed-bw stx)
         (type-set (syntax/loc stx (query #:forall [typed-decl ...]
                                          #:bitwidth typed-bw
                                          #:ensure typed-form))
                   void)))]
    [(query #:forall [decl ...] #:ensure form)
     (parameterize ([current-env (env)])
       (with-syntax ([(typed-decl ...) (map typecheck-query-declaration (syntax->list #'(decl ...)))]
                     [typed-form       (typecheck #'form)])
         (type-set (syntax/loc stx (query #:forall [typed-decl ...] #:ensure typed-form)) void)))]
    [_ (raise-bad-form-error 
        (format "(~a #:forall [(: type id ...+) ...+] form)" (syntax->datum #'query)) 
        stx)])) 

; Typechecks the print statement.
(define (typecheck-print stx)
  (syntax-case stx ()
    [(print form ...)
     (let ([forms (for/list ([f (syntax->list #'(form ...))])
                    (let ([tf (typecheck f)])
                      (when (equal? void (type-ref tf))
                        (raise-bad-type-error "non-void type" void stx f))
                      tf))])
       (type-set (quasisyntax/loc stx (print #,@forms)) void))]))

; Typechecks the if statement.
(define (typecheck-if-statement stx)
  (syntax-case stx ()
    [(if test { then ... } { else ... })
     (let ([test-expr (typecheck #'test)]
           [then-body (parameterize ([current-env (env)])
                        (map typecheck (syntax->list #'(then ...))))]
           [else-body (parameterize ([current-env (env)])
                        (map typecheck (syntax->list #'(else ...))))])
       (check-implicit-conversion (type-ref test-expr) bool stx #'test)
       (type-set
        (quasisyntax/loc stx
          (if #,test-expr #,then-body #,else-body))
        void))]
    [(if test { then ... }) (typecheck-if-statement (syntax/loc stx (if test {then ...} {})))]
    [_ (raise-bad-form-error "(if test { thenExpr ...}) or (if test { thenExpr ...} { elseExpr ...})" stx)]))

; Typechecks the range declaration.
(define (typecheck-range-declaration stx)
  (syntax-case stx ()
    [(: type var in (range arg ...))
     (and (identifier? #':) (equal? (syntax->datum #':) ':)
          (type-identifier? #'type) (equal? (identifier->type #'type stx) int)
          (identifier? #'in) (equal? (syntax->datum #'in) 'in)
          (identifier? #'range) (equal? (syntax->datum #'range) 'range)
          (let ([len (length (syntax->list #'(arg ...)))])
            (and (rosette/<= 1 len) (rosette/<= len 3))))
     (let ([args (map typecheck (syntax->list #'(arg ...)))])
       (for ([a args])
         (check-no-conversion (type-ref a) int a stx))
       (bind #'var int stx)
       (type-set (quasisyntax/loc stx (: type #,(lookup #'var) in (range #,@args))) int))]
    [_  (raise-bad-form-error "(: int var in (range [start] end [step]))" stx)]))
  
; Typechecks the for statement.
(define (typecheck-for-statement stx)
  (syntax-case stx ()
    [(for [decls ...] expr ...)
     (parameterize ([current-env (env)])
       (let* ([ds (map typecheck-range-declaration (syntax->list #'(decls ...)))]
              [body (map typecheck (syntax->list #'(expr ...)))])
         (type-set
          (quasisyntax/loc stx 
            (for [#,@ds] #,@body))
          void)))]
    [_ (raise-bad-form-error "(for [(: int var in rangeExpr) ...] expr ...)" stx)]))

; Typechecks the sizeof expression.
(define (typecheck-sizeof stx)
  (syntax-case stx ()
    [(sizeof type)
     (let ([t  (identifier->type #'type stx)])
       (unless (real-type? t)
         (raise-bad-type-error "real type" t stx #'type))
       (type-set stx int))]
    [_ (raise-bad-form-error "(sizeof realType)" stx)]))

; Typechecks the addressof expression.
(define (typecheck-addressof stx)
  (syntax-case stx ()
    [(@ val)
     (identifier? #'val)
     (let* ([v (typecheck #'val)]
            [t (type-ref v)]) 
       (unless (real-type? t)
         (raise-bad-type-error "real lvalue" t stx #'val))
       (type-set (quasisyntax/loc stx (@ #,v)) (base->pointer-type t)))]
    [_ (raise-bad-form-error "(@ identifier)" stx)]))

; Typechecks a procedure or kernel.
(define (typecheck-procedure stx)
  (syntax-case stx ()
    [(proc out (id [type param] ...) expr ...)
     (parameterize ([current-env (env)])
       (for-each typecheck (syntax->list #`([: type param] ...)))
       (let ([out-type (function-type-result (type-ref (typecheck #'id)))]
             [exprs (map typecheck (syntax->list #'(expr ...)))])
         (if (null? exprs)
             (check-no-conversion out-type void stx)
             (unless (equal? out-type void)
               (check-no-conversion (type-ref (last exprs)) out-type stx (last exprs))))
         (type-set
          (quasisyntax/loc stx
            (proc out (id [type param] ...) #,@exprs))
          void)))]
    [_ (raise-bad-form-error (format "(~a type (id [type id] ...) expr ...)" (syntax->datum stx)) stx)]))

; Typechecks a grammar declaration.
(define (typecheck-grammar stx)
  (syntax-case stx ()   
    [(grammar out (id [type param] ...) expr)
     (parameterize ([current-env (env)])
       (for-each typecheck (syntax->list #`([: type param] ...)))
       (let ([out-type (function-type-result (type-ref (typecheck #'id)))]
             [texpr (typecheck #'expr)])
         (unless (equal? out-type void)
           (check-no-conversion (type-ref texpr) out-type stx texpr))
         (type-set
          (quasisyntax/loc stx
            (grammar out (id [type param] ...) #,texpr))
          void)))]
    [_ (raise-bad-form-error (format "(~a type (id [type id] ...) expr ...)" (syntax->datum stx)) stx)]))

; Typechecks the ternary selection operator.
(define (typecheck-selection stx)
  (syntax-case stx ()
    [(?: a b c)
     (let* ([ta (typecheck #'a)]  
            [t0 (type-ref ta)]
            [tb (typecheck #'b)]
            [t1 (type-ref tb)]
            [tc (typecheck #'c)]
            [t2 (type-ref tc)])
       (define out
         (cond [(scalar-type? t0) (if (equal? t1 t2) t1 (common-real-type t1 t2))]
               [(vector-type? t0) (common-real-type t0 t1 t2)]
               [else (raise-syntax-error #f "not a real type" stx #'a)]))
       (unless out (raise-no-common-type-error stx))
       (type-set (quasisyntax/loc stx (?: #,ta #,tb #,tc)) out))]
    [_ (raise-operator-arity-error "3" stx)]))
           

; Typechecks operators that accept operands with a common real type and 
; produce that type as the output.  
(define (typecheck-real-operator stx)
  (syntax-case stx ()
    [(op arg ...)
     (let ([args (map typecheck (syntax->list #'(arg ...)))])
       (define crt (apply common-real-type (map type-ref args)))
       (unless crt (raise-no-common-type-error stx))
       (type-set (quasisyntax/loc stx (op #,@args)) crt))]))

; Typechecks operators that accept operands with a common integer type and 
; produce that type as the output.  
(define (typecheck-int-operator stx)
  (syntax-case stx ()
    [(op arg ...)
     (let ([args (map typecheck (syntax->list #'(arg ...)))]) 
       (define crt (apply common-real-type (map type-ref args)))
       (unless (and crt (or (equal? crt int) (equal? (type-base crt) int)))
         (raise-no-common-type-error stx "integer"))
       (type-set (quasisyntax/loc stx (op #,@args)) crt))]))

; Typechecks operators that accept bool operands and produce a bool output.
(define (typecheck-bool-operator stx)
  (syntax-case stx ()
    [(op arg ...)
     (let ([args (map typecheck (syntax->list #'(arg ...)))])
       (for ([a args]) 
         (check-implicit-conversion (type-ref a) bool stx))
       (type-set (quasisyntax/loc stx (op #,@args)) bool))]))

; Typechecks operators that accept operands with a common real type and 
; produce an int type as the output.  
(define (typecheck-comparison-operator stx)
  (syntax-case stx ()
    [(op arg ...)
     (let ([args (map typecheck (syntax->list #'(arg ...)))])
       (define crt (apply common-real-type (map type-ref args)))
       (unless crt (raise-no-common-type-error stx))
       (type-set (quasisyntax/loc stx (op #,@args)) 
                 (base->real-type int (real-type-length crt))))]))

; Typechecks a cast.
(define (typecheck-cast stx)
  (syntax-case stx ()
    [((type) val)
     (let ([t (identifier->type #'type stx)]
           [v (typecheck #'val)])
       (check-implicit-conversion (type-ref v) t stx)
       (type-set (quasisyntax/loc stx ((type) #,v)) t))]
    [_ (raise-bad-form-error "((type) expr)" stx)]))

; Typechecks the choose expression.
(define (typecheck-choose stx)
  (syntax-case stx ()
    [(choose expr rest ...)
     (let* ([x (typecheck #'expr)]
            [t (type-ref x)]
            [xs (map typecheck (syntax->list #'(rest ...)))])
       (unless (andmap (compose1 (curry equal? t) type-ref) xs)
         (raise-bad-type-error "choices of the same type" (cons t (map type-ref xs)) stx))
       (type-set (quasisyntax/loc stx (choose #,x #,@xs)) t))]
    [_ (raise-bad-form-error "(choose expr ...+)" stx)]))

; Typechecks the ?? expression.
(define (typecheck-?? stx)
  (syntax-case stx ()
    [(?? type)
     (type-identifier? #'type)
     (let ([t (identifier->type #'type stx)])
       (unless (scalar-type? t)
         (raise-bad-type-error "scalar type" t stx))
       (type-set stx t))]
    [_ (raise-bad-form-error "(?? scalarType)" stx)]))

; Typechecks a form that is not a datum or an identifier.
(define typecheck-form
  (let* ([procs (make-free-id-table #:phase 10)])
    
    (dict-set! procs #'assert         typecheck-assertion)
    (dict-set! procs #'verify         typecheck-query)
    (dict-set! procs #'synth          typecheck-query)
    (dict-set! procs #'choose         typecheck-choose)
    (dict-set! procs #'??             typecheck-??)
    (dict-set! procs #'grammar        typecheck-grammar)
    
    (dict-set! procs #'procedure      typecheck-procedure)
    (dict-set! procs #'kernel         typecheck-procedure)
    
    (dict-set! procs #':              typecheck-declaration)
    (dict-set! procs #'sizeof         typecheck-sizeof)
    (dict-set! procs #'@              typecheck-addressof)
    (dict-set! procs #'=              typecheck-assignment)
    (dict-set! procs #'if             typecheck-if-statement)
    (dict-set! procs #'for            typecheck-for-statement)
    (dict-set! procs #'locally-scoped typecheck-locally-scoped)
    (dict-set! procs #'print          typecheck-print)
    
    (dict-set! procs #'?:             typecheck-selection)
    (for ([op real-operators])        (dict-set! procs op typecheck-real-operator))
    (for ([op int-operators])         (dict-set! procs op typecheck-int-operator))
    (for ([op bool-operators])        (dict-set! procs op typecheck-bool-operator))
    (for ([op comparison-operators])  (dict-set! procs op typecheck-comparison-operator))
     
    (lambda (stx)
      (syntax-case stx ()
        [((tag) _ ...) (type-identifier? #'tag)
                       (typecheck-cast stx)]
        [(tag _ ...)   (and (identifier? #'tag) (dict-has-key? procs #'tag))
                       ((dict-ref procs #'tag) stx)]
        [(_ _ ...)     (typecheck-app-or-ref stx)]))))

