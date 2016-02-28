#lang rosette

(require (for-syntax "types.rkt" "errors.rkt" (only-in racket/syntax with-syntax*)) 
         "types.rkt" (prefix-in model/ "../model/operators.rkt"))

(provide ?: 
         + - * / pow sqrt abs real-operators       
         % & $ ^ ~ << >>      int-operators       
         ! && ||              bool-operators       
          == != < > <= >=     comparison-operators)

(define-syntax (define-operator stx)
  (syntax-case stx ()
    [(_ id impl [pat ... rest dots])
     (equal? '... (syntax->datum #'dots))
     (let ([msg (format ">= ~a" (length (syntax->list #'(pat ...))))])
       #`(define-syntax (id stx)
           (syntax-case stx ()
             [(_ pat ... rest dots)
              (let ([out (type-ref stx)])
                (with-syntax ([crt (type-name out)])
                  (if (scalar-type? out)
                      (syntax/loc stx 
                        (impl ((crt) pat) ... ((crt) rest) dots))
                      (syntax/loc stx 
                        (model/apply-operator crt impl ((crt) pat) ... ((crt) rest) dots)))))]
             [_ (raise-operator-arity-error #,msg stx)])))]
    [(_ id impl [pat ...])
     (let ([msg (format "~a" (length (syntax->list #'(pat ...))))])
       #`(define-syntax (id stx)
           (syntax-case stx ()
             [(_ pat ...)
              (let ([out (type-ref stx)])
                (with-syntax ([crt (type-name out)])
                  (if (scalar-type? out)
                      (syntax/loc stx (impl ((crt) pat) ...))
                      (syntax/loc stx (model/apply-operator crt impl ((crt) pat) ...)))))]
             [_ (raise-operator-arity-error #,msg stx)])))]))

(define-syntax-rule (define-comparator id impl)
  (define-syntax (id stx)
    (syntax-case stx ()
      [(_ x y)
       (let ([out (type-ref stx)])
         (with-syntax ([intk (type-name out)]
                       [crt  (type-name (common-real-type (type-ref #'x) (type-ref #'y)))])
           (if (scalar-type? out)
               (syntax/loc stx
                 ((intk) (impl ((crt) x) ((crt) y))))
               (quasisyntax/loc stx
                 (model/apply-comparison intk impl ((crt) x) ((crt) y))))))]
      [_ (raise-operator-arity-error "2" stx)])))

(define-syntax (?: stx)
  (syntax-case stx ()
    [(?: a b c)
     (with-syntax ([out (type-name (type-ref stx))])
       (if (scalar-type? (type-ref #'a))
           (syntax/loc stx (model/?: ((bool) a) ((out) b) ((out) c)))
           (syntax/loc stx (model/apply-selector out ((out) a) ((out) b) ((out) c)))))]
    [_ (raise-operator-arity-error "3" stx)]))

(define-operator +    model/+    [x y ...])
(define-operator -    model/-    [x y ...])
(define-operator *    model/*    [x y z ...])
(define-operator /    model//    [x y])
(define-operator pow  model/pow  [x y])
(define-operator sqrt model/sqrt [x])
(define-operator abs  model/abs [x])

(define-operator %  model/% [x y])
(define-operator &  model/& [x y z ...])
(define-operator $  model/$ [x y z ...])
(define-operator ^  model/^ [x y z ...])
(define-operator ~  model/~ [x])
(define-operator << model/<< [x y])
(define-operator >> model/>> [x y])

(define-operator !  model/!  [x])
(define-operator && model/&& [x y z ...])
(define-operator || model/|| [x y z ...])

(define-comparator == model/==)
(define-comparator != model/!=)
(define-comparator <  model/<)
(define-comparator >  model/>)
(define-comparator <= model/<=)
(define-comparator >= model/>=)

(define real-operators       (syntax->list #'(+ - * / pow sqrt abs)))
(define int-operators        (syntax->list #'(% & $ ^ ~ << >>)))
(define bool-operators       (syntax->list #'(! && ||)))
(define comparison-operators (syntax->list #'(== != < > <= >=)))
         
