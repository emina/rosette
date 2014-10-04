#lang racket

(require "location.rkt" "instruction.rkt" 
         rosette/base/define
         rosette/base/num
         (for-syntax racket/syntax))

(provide library)

(define-syntax-rule (library c ...) 
  (begin
    (define-locations (+ (component-count c) ...))
    (append (component c) ...)))

(define-syntax component-count 
  (syntax-rules ()
    [(_ [form ...]) (component-count form ...)]
    [(_ _ #:arity _ #:count c) c]
    [(_ _ c) c]
    [(_ _) 1]))

(define-syntax (component stx)
  (syntax-case stx ()
    [(_ [form ...]) #'(component form ...)]
    [(_ id #:arity k #:count c)
     (const? #'id)
     (let ([input-value (format-id #'id "~a-value" #'id #:source #'id)]
           [output (format-id #'id "~a-output" #'id #:source #'id)])
       #`(local [(define-symbolic #,input-value @number? [c])
                 (define-symbolic #,output location? [c])]
           (map (lambda (val out) (new-instruction #'id (const val) '() out))
                #,input-value #,output)))]
    [(_ id #:arity k #:count c)
     (or (identifier? #'id)
         (raise-syntax-error "expected a component identifer, given " #'id))
     (let ([inputs (format-id #'id "~a-input" #'id #:source #'id)]
           [output (format-id #'id "~a-output" #'id #:source #'id)])
       #`(local [(define-symbolic #,inputs location? [c k])
                 (define-symbolic #,output location? [c])]
           (map (lambda (in out) (new-instruction #'id id in out))
                (if (null? #,inputs) (make-list c '()) #,inputs) #,output)))]
    [(_ id c) #'(case (procedure-arity id)
                  [(0) (component id #:arity 0 #:count c)]
                  [(1) (component id #:arity 1 #:count c)]
                  [(2) (component id #:arity 2 #:count c)]
                  [(3) (component id #:arity 3 #:count c)]
                  [else (error 'component "expected a procedure of arity between 0 and 3, given ~a" id)])]
    [(_ id) #'(component id #:count 1)]))
             
(define-for-syntax (const? id)
   (let ([id-binding (identifier-binding id)]
         [const-binding (identifier-binding #'const)])
     (and id-binding
          (equal? (cadr id-binding) (cadr const-binding))
          (equal? (module-path-index-resolve (car id-binding))
                  (module-path-index-resolve (car const-binding))))))
