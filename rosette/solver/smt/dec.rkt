#lang racket

(require (only-in "smtlib2.rkt" true false)
         (only-in "../../base/core/term.rkt" @app  constant? term-type solvable-default)
         (only-in "../../base/core/function.rkt" fv function? function-domain function-range)
         (only-in "../../base/core/polymorphic.rkt" ite)
         (only-in "../../base/core/equality.rkt" @equal?)
         (only-in "../../base/core/bool.rkt" @! @&& @|| @=> @<=> @boolean?)
         (only-in "../../base/core/real.rkt" 
                  @integer? @real? @= @< @<= @>= @> 
                  @+ @* @- @/ @integer->real @real->integer @int?)
         (only-in "../../base/core/bitvector.rkt" 
                  bitvector? bitvector bv bv? bitvector-size 
                  @bvslt @bvsle @bvult @bvule   
                  @bvnot @bvor @bvand @bvxor @bvshl @bvlshr @bvashr
                  @bvneg @bvadd @bvmul @bvudiv @bvsdiv @bvurem @bvsrem @bvsmod
                  @concat @extract))


(provide decode-model)

; Decodes a sol hashmap into a map from constants declared in env to
; their values, as given by sol. The env argument is assumed to be a
; dictionary from constant? to symbol?.
(define (decode-model env sol)
  (let ([i-sol (inline (α-rename sol) env)])
    (for/hash ([(decl id) (in-dict env)]
               #:when (and (constant? decl) (hash-has-key? i-sol id)))
      (values decl (interpret (hash-ref i-sol id) (term-type decl)))))) 


; Given an expression produced by the inline procedure, along with the type of
; its corresponding Rosette constant?, returns the Rosette value encoded
; by the given expression.
(define (interpret expr type-or-env)
  (match expr
    [(list (== 'λ) params body)
     (fv type-or-env
         (procedure-reduce-arity
          (lambda args
            (interpret
             body
             (for/hash ([p params] [a args])
               (values p a))))
          (length params)))]
    [(list op args ...)
     (apply op (for/list ([arg args])
                 (interpret arg type-or-env)))]
    [(? symbol?) (hash-ref type-or-env expr)]
    [_ expr]))

; Given a map M from symbols to SMTLib function definitions of the form
; (define-fun id ((param type) ...) ret body), this procedure applies alpha-renaming
; to each definition in M.  In particular, it maps each key K in M 
; to a definition (define-fun id ((param' type) ...) ret body') such that
; every param in M[K] is replaced with a fresh symbol param', and
; body' is obtained by substituting param' for every occurrence of param in the body of M[K]. 
(define (α-rename sol)
  (for/hash ([(k v) sol])
    (values
     k        
     (match v
       [(list (== 'define-fun) id '() ret body)
        `(define-fun ,id () ,ret ,(substitute body (hash)))]
       [(list (== 'define-fun) id params ret body)
        (let ([α-params (for/list ([p params]) (gensym (car p)))])
          `(define-fun
             ,id
             ,(for/list ([α α-params] [p params])
                `(,α ,(cadr p)))
             ,ret
             ,(substitute body (for/hash ([p params] [α α-params])
                                 (values (car p) α)))))]))))

; Given an s-expression B and a map M from symbols to values,
; returns a B' that replaces each occurrence of a key K in M with M[K].
(define (substitute body env)
  (match body
    [(list (== 'let) binds body)
     (let ([α-ids (for/list ([id:expr binds]) (gensym (car id:expr)))])
       `(let ,(for/list ([α-id α-ids][id:expr binds])
                (list α-id (substitute (cadr id:expr) env)))
          ,(substitute
            body
            (apply hash-set* env (for/fold ([out '()]) ([id:expr binds][α-id α-ids])
                                   `(,(car id:expr) ,α-id ,@out))))))]
    [(list es ...) (for/list ([e es]) (substitute e env))]
    [e (if (hash-has-key? env e) (hash-ref env e) e)]))

; Given an alpha-renamed sol map and an env dictionary from terms to symbols,
; maps each key K in sol to an s-expression that encodes a Rosette value corresponding to sol[K]. 
; In particular, if env maps a function? to K, then the result maps K to an s-expression
; of the form (λ (id ...) body) such that body replaces all free identifiers the body of
; sol[K] with a corresponding Rosette procedure.
; Otherwise, the result maps K to a boolean?, integer?, real?, bv?, or an s-expression
; of the form (op arg ...) where op is a Rosette procedure and arg is either a constant value or
; another s-expression of the same form. 
(define inline
  (case-lambda
    [(sol env)
     (define ~env (hash-copy optable))
     (for ([(decl id) (in-dict env)]
           #:when (and (constant? decl) (not (hash-has-key? sol id))))
       (hash-set! ~env id decl))
     (for/hash ([(k v) sol])
       (values k (inline v sol ~env)))]
    [(expr sol ~env)
     (match expr
       [(list (== 'define-fun) id '() _ body)
        (unless (hash-has-key? ~env id)
          (hash-set! ~env id (inline body sol ~env)))
        (hash-ref ~env id)]
       [(list (== 'define-fun) id params _ body)
        (unless (hash-has-key? ~env id)
          (hash-set! ~env id `(λ ,(map car params) ,(inline body sol ~env))))
        (hash-ref ~env id)]
       [(== true) #t]
       [(== false) #f]
       [(? integer?) (inexact->exact expr)]
       [(? real?) expr]
       [(? symbol?)
        (cond
          [(hash-has-key? ~env expr) (hash-ref ~env expr)]
          [(hash-has-key? sol expr)  (inline (hash-ref sol expr) sol ~env)]
          [else
           (match (symbol->string expr)
             [(regexp #px"(\\d*\\.?\\d+)\\?" (list _ (app string->number r))) r]
             [(regexp #px"#b(\\d+)" (list str (app string-length len))) (bv (string->number str) (bitvector len))]
             [(regexp #px"#x(.+)" (list str (app string-length len))) (bv (string->number str) (bitvector (* 4 len)))]
             [_ expr])])]
       [(list (== '_) (app symbol->string (regexp #px"bv(\\d+)" (list _ (app string->number n)))) len)
        (bv n (bitvector len))]
       [(list (list (== '_) (== 'extract) i j) s)
        `(, @extract ,(inline i sol ~env) ,(inline j sol ~env) ,(inline s sol ~env))]
       [(list (== 'let) binds body)
        (substitute (inline body sol ~env)
                    (for/hash ([id:expr binds])
                      (values (car id:expr) (inline (cadr id:expr) sol ~env))))]
       [(list op args ...)
          (match (inline op sol ~env)
            [(and (or (? procedure?) (? constant?)) ~op)
             `(,~op ,@(for/list ([arg args]) (inline arg sol ~env)))]
            [(list (== 'λ) params body)
             (substitute body (for/hash ([p params] [arg args])
                                (values p (inline arg sol ~env))))]
            [(== 'root-obj)
             (error (format
                     "~a
                     The solver returned an algebraic real number as part of the solution.
                     Algebraic numbers cannot be precisely represented by Rosette or Racket.
                     To obtain a solution that approximates all real numbers to N decimal places,
                     re-solve the constraints using a Z3 instance with the following options:
                     (z3 #:options (hash ':pp.decimal 'true ':pp.decimal-precision N))"
                     expr))]
            [sym (error "Unrecognized symbol: " sym)])])]))
              
(define optable
  (hash '= @equal? 'ite ite
        'not @! 'and @&& 'or @|| '=> @=> 'implies @=> '<=> @<=> 'iff @<=>
        '- @- '/ @/ '+ @+ '* @* '< @< '<= @<= '>= @>= '> @>
        'to_real @integer->real 'to_int @real->integer 'is_int @int?
        'bvslt @bvslt 'bvsle @bvsle 'bvult @bvult 'bvule @bvule   
        'bvnot @bvnot 'bvor @bvor 'bvand @bvand 'bvxor @bvxor
        'bvshl @bvshl 'bvlshr @bvlshr 'bvashr @bvashr
        'bvneg @bvneg 'bvadd @bvadd 'bvmul @bvmul
        'bvudiv @bvudiv 'bvsdiv @bvsdiv
        'bvurem @bvurem 'bvsrem @bvsrem
        'bvsmod @bvsmod 'concat @concat))   
