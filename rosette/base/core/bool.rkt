#lang racket

(require "term.rkt" "union.rkt" "exn.rkt" "result.rkt")

(provide
 ;; ---- lifted boolean? operations ---- ;;
 @boolean? @false? @true?
 ! && || => <=> @! @&& @|| @=> @<=> @exists @forall
 and-&& or-|| instance-of? T*->boolean?
 ;; ---- VC generation ---- ;;       
 @assert @assume $assert $assume
 (rename-out [get-vc vc]) clear-vc! with-vc merge-vc!
 spec? spec-assumes spec-asserts
 spec-tt spec-tt?)

;; ----------------- Boolean type ----------------- ;; 
(define-lifted-type @boolean? 
  #:base boolean?
  #:is-a? (instance-of? boolean? @boolean?)
  #:methods
  [(define (solvable-default self) #f)
   (define (type-eq? self u v) (<=> u v)) 
   (define (type-equal? self u v) (<=> u v))
   (define (type-cast self v [caller 'type-cast])
     (match v
       [(? boolean?) v]
       [(term _ (== self)) v]
       [(union : [g (and (or (? boolean?) (term _ (== self))) u)] _ ...)
        ($assert g (argument-error caller "boolean?" v))
        u]
       [_  ($assert #f (argument-error caller "boolean?" v))]))
   (define (type-compress self force? ps)
     (match ps
       [(list _) ps]
       [(list (cons g v) (cons u w)) (list (cons (|| g u) (|| (&& g v) (&& u w))))]
       [_ (list (cons (apply || (map car ps)) 
                      (apply || (for/list ([p ps]) (&& (car p) (cdr p))))))]))])

;; ----------------- Lifting utilities ----------------- ;; 

(define (lift-op op)
  (define caller (object-name op))
  (case (procedure-arity op)
    [(1)  (lambda (x) (op (type-cast @boolean? x caller)))]
    [(2)  (lambda (x y) (op (type-cast @boolean? x caller) (type-cast @boolean? y caller)))]
    [else (case-lambda [() (op)]
                       [(x) (op (type-cast @boolean? x caller))]
                       [(x y) (op (type-cast @boolean? x caller) (type-cast @boolean? y caller))]
                       [xs (apply op (for/list ([x xs]) (type-cast @boolean? x caller)))])]))

; A generic typing procedure for a lifted operator that takes N >= 0 arguments of type T
; and returns a @boolean?. See term.rkt.
(define (T*->boolean? . xs) @boolean?)

(define-syntax-rule (define-lifted-operator @op $op)
  (define-operator @op
    #:identifier '$op
    #:range T*->boolean?
    #:unsafe $op
    #:safe (lift-op $op)))

(define-syntax-rule (define-quantifier $op @op)
  (begin
    (define $op (quantifier @op))
    (define-operator @op
      #:identifier '$op
      #:range T*->boolean?
      #:unsafe $op
      #:safe
      (lambda (@vars @body)
        (match* (@vars (type-cast @boolean? @body '$op))
          [((list (constant _ (? primitive-solvable?)) (... ...)) body)
           ($op @vars body)]
          [(_ _)
           ($assert
            #f
            (argument-error '$op "list of symbolic constants of primitive solvable types" @vars))])))))

;; ----------------- Basic boolean operators ----------------- ;; 
(define (! x)
  (match x
    [(? boolean?) (not x)]
    [(expression (== @!) y) y]
    [_ (expression @! x)]))

(define && (logical-connective @&& @|| #t #f))
(define || (logical-connective @|| @&& #f #t))

(define (=> x y) ; (|| (! x) y))
  (cond
    [(equal? x y) #t]
    [(eq? x #f) #t]
    [(eq? y #t) #t]
    [(eq? x #t) y]
    [(eq? y #f) (! x)]
    [(cancel? x y) y]
    [else
     (define !x (! x))
     (match y
       [(expression (== @||) _ ... (== x) _ ...) #t]
       [(expression (== @||) (== !x) b) (=> x b)]
       [(expression (== @||) b (== !x)) (=> x b)]
       [(expression (== @&&) (== x) b) (=> x b)]
       [(expression (== @&&) b (== x)) (=> x b)]
       [(expression (== @&&) (expression (== @||) _ ... (== x) _ ...) b) (=> x b)]
       [(expression (== @&&) b (expression (== @||) _ ... (== x) _ ...)) (=> x b)]
       [(expression (== @<=>) (== x) b) (=> x b)]
       [(expression (== @<=>) b (== x)) (=> x b)]
       [_ (|| !x y)])]))
       
       
        

(define (<=> x y) ;(|| (&& x y) (&& (! x) (! y))))))
  (cond [(equal? x y) #t]
        [(boolean? x) (if x y (! y))]
        [(boolean? y) (if y x (! x))]
        [(cancel? x y) #f]
        [(term<? x y) (expression @<=> x y)]
        [else         (expression @<=> y x)]))

(define-lifted-operator @! !)
(define-lifted-operator @&& &&)
(define-lifted-operator @|| ||)
(define-lifted-operator @=> =>)
(define-lifted-operator @<=> <=>)

(define (@false? v)
  (match v
    [#f #t]
    [(term _ (== @boolean?)) (! v)]
    [(union xs (== @any/c))
     (let loop ([xs xs])
       (match xs
         [(list) #f]
         [(list (cons g (and (or (? boolean?) (term _ (== @boolean?))) u)) _ ...)
          (&& g (! u))]
         [_ (loop (cdr xs))]))]
    [_ #f]))

(define (@true? v)
  (or (eq? #t v) (! (@false? v))))

(define-quantifier exists @exists)
(define-quantifier forall @forall)


       
;; ----------------- Additional operators ----------------- ;; 
(define-syntax and-&&
  (syntax-rules ()
    [(_) #t]
    [(_ v0) v0]
    [(_ v0 #:rest (r ...)) (let ([t0 v0]) (and t0 (@&& r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) (let ([t0 v0]) (and t0 (and-&& v ... #:rest (r ... t0))))]
    [(_ v0 v ...) (let ([t0 v0]) (and t0 (and-&& v ... #:rest (t0))))]))

(define-syntax or-||
  (syntax-rules ()
    [(_) #f]
    [(_ v0) v0]
    [(_ v0 #:rest (r ...)) (let ([t0 v0]) (or (equal? #t t0) (@|| r ... t0)))]
    [(_ v0 v ... #:rest (r ...)) (let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (r ... t0))))]
    [(_ v0 v ...) (let ([t0 v0]) (or (equal? #t t0) (or-|| v ... #:rest (t0))))]))

(define-syntax-rule (instance-of? primitive-type ... symbolic-type)
  (match-lambda [(? primitive-type) #t] ...
                [(and (? typed? v) (app get-type t)) 
                 (or (and t (subtype? t symbolic-type)) 
                     (and (union? v) (apply || (for/list ([g (in-union-guards v symbolic-type)]) g))))]
                [_ #f]))


;; ----------------- Partial evaluation rules for ∀ and ∃ ----------------- ;;

(define-syntax-rule (quantifier @op)
  (lambda (vars body)
    (match* (vars body)
      [((list) _) body]
      [(_ (? boolean?)) body]
      [(_ _) (expression @op vars body)])))

;; ----------------- Partial evaluation rules for && and || ----------------- ;; 
(define-syntax-rule (logical-connective op co iden !iden)
  (case-lambda 
    [() iden]
    [(x) x]
    [(x y) 
     (match* (x y)
       [((== iden) _) y]
       [(_ (== iden)) x]
       [((== !iden) _) !iden]
       [(_ (== !iden)) !iden]
       [(_ _)
        (first-value 
         (simplify-connective op co !iden x y)
         (if (term<? x y)  (expression op x y) (expression op y x)))])]
    [xs 
     (cond [(member !iden xs) !iden]
           [else 
            (match (simplify-fp op co !iden (remove-duplicates (filter term? xs)))
              [(list) iden]
              [(list x) x]
              [ys (apply expression op (sort ys term<?))])])]))
       
(define ⊥ (void))
(define-syntax first-value
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (let ([v e0])
                    (if (void? v)
                        (first-value e ...)
                        v))]))

(define (simplify-connective op co !iden x y)
  (match* (x y)
    [(_ (== x)) x]
    [((? expression?) (? expression?))
     (first-value
      (simplify-connective:expr/any op co !iden x y)
      (simplify-connective:expr/any op co !iden y x)
      (simplify-connective:expr/expr op co !iden x y))]
    [((? expression?) _)
     (simplify-connective:expr/any op co !iden x y)]
    [(_ (? expression?))
     (simplify-connective:expr/any op co !iden y x)]
    [(_ _) ⊥]))
      
(define (simplify-connective:expr/any op co !iden x y)
  (match x 
    [(expression (== @!) (== y)) !iden]
    [(expression (== co) _ ... (== y) _ ...) y]
    [(expression (== co) (expression (== @!) (== y)) b) (op y b)]
    [(expression (== co) b (expression (== @!) (== y))) (op y b)]
    [(expression (== op) _ ... (== y) _ ...) x]
    [(expression (== op) _ ... (expression (== @!) (== y)) _ ...) !iden]
    [(expression (== @!) (expression (== co) _ ... (== y) _ ...)) !iden]
    [(expression (== @!) (expression (== co) _ ... (expression (== @!) (== y)) _ ...)) x]
    [(expression (== @!) (expression (== op) _ ... (expression (== @!) (== y)) _ ...)) y]
    [(expression (== @!) a) 
     (match y 
       [(expression (== op) _ ... (== a) _ ...) !iden]
       [(expression (== co) (== a) b) (op x b)]
       [(expression (== co) b (== a)) (op x b)]
       [_ ⊥])]
    [_  ⊥]))

; Applies the following simplification rules symmetrically:
; (1) (op (op a1 ... an) (op ai ... aj)) ==> (op a1 ... an)
; (2) (op (op a1 ... ai ... an) (op b1 ... (neg ai) ... bn) ==> !iden
; (3) (op (co a1 ... an) (co ai ... aj)) ==> (co ai ... aj)
; Returns ⊥ if none of the rules applicable; otherwise returns the simplified result.
(define (simplify-connective:expr/expr op co !iden a b)
  (match* (a b)
    [((expression (== op) xs ...) (expression (== op) ys ...))
     (cond [(sublist? xs ys) b]
           [(sublist? ys xs) a]
           [(for*/or ([x xs][y ys]) (cancel? x y)) !iden]
           [else ⊥])]
    [((expression (== co) xs ...) (expression (== co) ys ...))
     (cond [(sublist? xs ys) a]
           [(sublist? ys xs) b]
           [else ⊥])]
    [((expression (== op) xs ...) (expression (== co) ys ...))
     (cond [(for*/or ([x xs][y ys]) (equal? x y)) a]
           [else ⊥])]
    [((expression (== co) xs ...) (expression (== op) ys ...))
     (cond [(for*/or ([y ys][x xs]) (equal? x y)) b]
           [else ⊥])]
    [(_ _) ⊥]))
    
(define (simplify-fp op co !iden xs)
  (or
   (and (> (length xs) 10) xs)
   (let-values ([(!ys ys) (for/fold ([!ys '()][ys '()]) ([x xs])
                            (match x
                              [(expression (== @!) y) (values (cons y !ys) ys)]
                              [_ (values !ys (cons x ys))]))])
     (for/first ([!y !ys] #:when (member !y ys)) (list !iden)))
   (let outer ([xs xs])
     (match xs
       [(list x rest ..1)
        (let inner ([head rest] [tail '()])
          (match head
            [(list) (match (outer tail)
                      [(and (list (== !iden)) t) t]
                      [t (cons x t)])]
            [(list y ys ...)
             (match (simplify-connective op co !iden x y)
               [(== ⊥) (inner ys (cons y tail))]
               [(== !iden) (list !iden)]
               [v (outer (cons v (append ys tail)))])]))]
       [_ xs]))))
            
(define (cancel? a b)
  (match* (a b)
    [(_ (expression (== @!) (== a))) #t]
    [((expression (== @!) (== b)) _) #t]
    [(_ _) #f]))


;; ----------------- VC generation ----------------- ;;

; A spec consists of two (symbolic or concrete) @boolean?
; values representing the assumptions and assertions issued
; so far. A spec is legal if at least one of its assumes or
; asserts is true under all models.
(struct spec (assumes asserts) #:transparent)

; The true (top) spec.
(define spec-tt (spec #t #t))

(define (spec-tt? s) (equal? s spec-tt))

; A conjunction of specs s1 ... sn is a spec that conjoins their
; assumes and asserts elementwise, i.e., 
; (spec (s1.assumes && ... && sn.assumes) (s1.asserts && ... && sn.asserts))
(define spec-and
  (case-lambda
    [()  spec-tt]
    [(s) s]
    [(s1 s2)  (spec (&& (spec-assumes s1) (spec-assumes s2))
                    (&& (spec-asserts s1) (spec-asserts s2)))]
    [ss (spec (apply && (map spec-assumes ss))
              (apply && (map spec-asserts ss)))]))

; Guarding a spec s with a guard g, which must be a concrete or
; symbolic boolean, produces the spec (spec (g => s.assumes) (g => s.asserts)).
(define (spec-guard s g)
  (spec (=> g (spec-assumes s)) (=> g (spec-asserts s))))

; Returns (spec (s.assumes && (s.asserts => g) s.asserts). 
(define (assuming s g)  ; g must be a symbolic or concrete boolean
  (spec (&& (spec-assumes s) (=> (spec-asserts s) g)) (spec-asserts s)))

; Returns (spec s.assumes (s.asserts && (s.assumes => g))). 
(define (asserting s g) ; g must be a symbolic or concrete boolean 
  (spec (spec-assumes s) (&& (spec-asserts s) (=> (spec-assumes s) g))))

; The vc parameter keeps track of the current verification condition,
; which is an instance of spec?. The default vc is the true spec.
(define vc (make-parameter
            spec-tt
            (lambda (v) (unless (spec? v) (raise-argument-error 'vc "spec?" v)) v)))

; Returns the current vc, without exposing the parameter outside the module. 
(define (get-vc) (vc))

; Clears the current vc by setting it to the true spec.
(define (clear-vc!) (vc spec-tt))

; Takes as input a list of n guards and n specs and sets the current vc
; to (vc) && (spec-guard guard1 specs1) && ... && (spec-guard guardn specn).
; Then, it checks if either the assumes or the asserts of the resulting spec
; are false? and if so, throws either an exn:fail:svm:assume? or
; exn:fail:svm:assert? exception. This procedure assumes that at most one
; of the given guards is true in any model, and if vc asserts or assumes
; evaluate to #f under some model, then each of the specs is equivalent to
; vc under that model.  
(define (merge-vc! guards specs)
  (unless (null? specs)
    (define specm (apply spec-and (vc) (map spec-guard specs guards)))
    (vc specm)
    (when (false? (spec-assumes specm))
      (raise-exn:fail:svm:assume:core "contradiction"))
    (when (false? (spec-asserts specm))
      (raise-exn:fail:svm:assert:core "contradiction"))))

; Sets the current vc to (spec-proc (vc) g) where g is (@true? val).
; If g is #f or the resulting vc's spec-field value is #f,
; uses raise-exn throws an exn:fail:svm exception. 
(define-syntax-rule (vc-set! val msg spec-proc spec-field raise-exn)
  (let* ([guard (@true? val)]
         [specg (spec-proc (vc) guard)])
    (vc specg)
    (when (false? guard)
      (raise-exn msg))
    (when (false? (spec-field specg))
      (raise-exn "contradiction"))))

; Sets the current vc to (asserting (vc) g) where g is (@true? val).
; If g is #f or the resulting vc's asserts field is #f, throws an
; exn:fail:svm:assert exception of the given kind.  
(define-syntax-rule (vc-assert! val msg raise-kind)
  (vc-set! val msg asserting spec-asserts raise-kind))

; Sets the current vc to (assuming (vc) g) where g is (@true? val).
; If g is #f or the resulting vc's assumes field is #f, throws an
; exn:fail:svm:assume exception of the given kind. 
(define-syntax-rule (vc-assume! val msg raise-kind)
  (vc-set! val msg assuming spec-assumes raise-kind))

; The $assert form has three variants: ($assert val), ($assert val msg),
; and ($assert val msg kind), where val is the value being asserted, msg
; is the failure message, and kind is a procedure that returns a subtype of
; exn:fail:svm:assert. Default values for msg and kind are #f and
; raise-exn:fail:svm:assert:core, respectively.
; The first two variants of this form are used for issuing assertions from
; within the Rosette core. The third variant is used to implement the @assert
; form that is exposed to user code. An $assert call modifies the current vc to
; reflect the issued assertion. If the issued assertion or the spec-assert of the
; current vc reduce to #f, the call throws an exception of the given kind after
; updating the vc.
(define-syntax ($assert stx)
  (syntax-case stx ()
    [(_ val)          (syntax/loc stx ($assert val #f  raise-exn:fail:svm:assert:core))]
    [(_ val msg)      (syntax/loc stx ($assert val msg raise-exn:fail:svm:assert:core))]
    [(_ val msg kind) (syntax/loc stx (vc-assert! val msg kind))]))

; Analogous to the $assert form, except that it modifies the current vc to
; reflect the issued assumption.
(define-syntax ($assume stx)
  (syntax-case stx ()
    [(_ val)          (syntax/loc stx ($assume val #f   raise-exn:fail:svm:assume:core))]
    [(_ val msg)      (syntax/loc stx ($assume val msg  raise-exn:fail:svm:assume:core))]
    [(_ val msg kind) (syntax/loc stx (vc-assume! val msg kind))]))

; The @assert form modifies the current vc to reflect the issued assertion.
; The form has two variants (@assert val) and (@assert val msg), where val
; is the value being asserted and msg is the optional error message in case
; val is #f. This form is exposed to user code.
(define-syntax (@assert stx)
  (syntax-case stx ()
    [(_ val)     (syntax/loc stx ($assert val #f  raise-exn:fail:svm:assert:user))]
    [(_ val msg) (syntax/loc stx ($assert val msg raise-exn:fail:svm:assert:user))]))

; The @assume form modifies the current vc to reflect the issued assumption.
; The form has two variants (@assume val) and (@assume val msg), where val
; is the value being assume and msg is the optional error message in case
; val is #f. This form is exposed to user code.
(define-syntax (@assume stx)
  (syntax-case stx ()
    [(_ val)     (syntax/loc stx ($assume val #f  raise-exn:fail:svm:assume:user))]
    [(_ val msg) (syntax/loc stx ($assume val msg raise-exn:fail:svm:assume:user))]))

(define (halt-svm ex)
  (halt ex (vc)))

(define (halt-err ex) ; Treat an exn:fail? error as an assertion failure.
  (halt (make-exn:fail:svm:assert:err (exn-message ex) (exn-continuation-marks ex))
        (asserting (vc) #f)))

; The with-vc form has two variants, (with-vc body) and (with-vc vc0 body).
; The former expands into (with-vc (vc) body). The latter sets the current
; vc to vc0, evaluates the given body, returns the result, and reverts vc
; to the value it held before the call to with-vc.
;
; If the evaluation of the body terminates normally, (with-vc vc0 body)
; outputs (ans v s) where v is the value computed by the body, and s is 
; the specification (i.e., assumes and asserts) generated during the evaluation,
; with v0 as the initial specification. 
;
; If the evaluation of the body terminates abnormally with an exn:fail? exception,
; (with-vc vc0 body) outputs (halt v s) where v is an exn:fail:svm? exception
; that represents the cause of the abnormal termination, and s is the specification
; (i.e., assumes and asserts) generated during the evaluation, with v0 as
; the initial specification.
(define-syntax (with-vc stx)
  (syntax-case stx ()
    [(_ body) (syntax/loc stx (with-vc (vc) body))]
    [(_ vc0 body)
     (syntax/loc stx
       (parameterize ([vc vc0])
         (with-handlers ([exn:fail:svm? halt-svm]
                         [exn:fail?     halt-err])
           (ans body (vc)))))]))
