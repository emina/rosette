#lang racket

(require "eval.rkt" "state.rkt" "../base/assert.rkt" "../base/define.rkt" "cegis.rkt"
         "../config/log.rkt"
         (only-in "../base/reflect.rkt" symbolics)
         (only-in "../base/bool.rkt" || !)
         (only-in "../base/equality.rkt" @equal?)
         (only-in "../base/term.rkt" expression constant? typed? get-type type-deconstruct)
         "../solver/solver.rkt"  "../solver/solution.rkt")

(provide solve solve/evaluate solve+ solve-all verify synthesize symbolics)

(define-syntax-rule (solve/evaluate form)  
  (let-values ([(val asserts) 
                (with-handlers ([exn:fail? (lambda (e) (values #f '(#f)))])
                  (with-asserts form))])
    (send (current-solver) clear)
    (evaluate val (solve-asserts asserts))))

(define-syntax-rule (solve form)  
  (let ([asserts (with-handlers ([exn:fail? always-unsat])
                   (with-asserts-only form))])
    (send (current-solver) clear)
    (solve-asserts asserts)))

(define-syntax-rule (solve+ form)  
  (let ([asserts (with-handlers ([exn:fail? always-unsat])
                   (with-asserts-only form))])
    (solve-asserts asserts)))

(define (solve-asserts asserts)
  (if (andmap passes? asserts)
      (current-solution)
      (begin 
        (send/apply (current-solver) assert asserts)
        (let ([sol (send/handle-breaks (current-solver) solve)])
          (unless (sat? sol) 
            (error 'solve "no satisfying execution found"))
          (current-solution sol)
          sol))))

(define-syntax-rule (solve-all form)  
  (let-values ([(val asserts) (with-asserts form)])
    (send (current-solver) clear)
    (send/apply (current-solver) assert asserts)
    (let loop ()
      (let ([sol (send/handle-breaks (current-solver) solve)])
        (cond [(sat? sol) 
               (send (current-solver) 
                     assert
                     (apply || (for/list ([binding (solution->list sol)])
                                 (! (@equal? (car binding) (cdr binding))))))
               (cons sol (loop))]
              [else (send (current-solver) clear)
                    '()])))))

(define-syntax verify
  (syntax-rules ()
    [(_ #:assume assume #:guarantee form)
     (let ([assumes (with-handlers ([exn:fail? always-false]) (with-asserts-only assume))]
           [asserts (with-handlers ([exn:fail? always-false]) (with-asserts-only form))])
       (when (null? asserts) 
         (error 'verify "no counterexample found"))
       (cond [(equal? assumes '(#f))
              (error 'verify "no counterexample found")]
             [(and (andmap passes? assumes) (ormap fails? asserts))
              (void)]
             [else 
              (send (current-solver) clear)
              (send/apply (current-solver) assert assumes)
              (send (current-solver) assert (apply || (map ! asserts)))
              (let ([sol (send/handle-breaks (current-solver) solve)])
                (send (current-solver) clear)
                (unless (sat? sol)
                  (error 'verify "no counterexample found"))
                (current-solution sol))])
       (current-solution))]
    [(_ form) (verify #:assume #t #:guarantee form)]))

(define-syntax synthesize 
  (syntax-rules (synthesize)
    [(_ #:forall inputs #:init init #:assume assume #:guarantee spec)
     (let*-values ([(free-vars) (symbolics inputs)]
                   [(init-sol) init]
                   [(assume-out assumes) (with-asserts assume)]
                   [(spec-out asserts)   (with-asserts spec)]
                   [(val cpu real gc)    (time-apply exists-forall `(,free-vars ,assumes ,asserts ,init-sol))])      
       (log-info "synthesis time (ms): cpu = ~a, real = ~a, gc = ~a" cpu real gc)
       (current-solution))]
    
    [(_ #:forall inputs #:assume assume #:guarantee spec)
     (synthesize #:forall inputs #:init (empty-solution) #:assume assume #:guarantee spec)]
    
    [(_ #:forall inputs #:init init #:guarantee spec)
     (synthesize #:forall inputs #:init init #:assume #t #:guarantee spec)]
     
    [(_ #:forall inputs #:guarantee spec)
     (synthesize #:forall inputs #:init (empty-solution) #:assume #t #:guarantee spec)]
      
    [(_ ([id rest ... type] ...) #:init init #:assume assume #:guarantee spec) 
     (local ([define-symbolic* id rest ... type] ...)
       (synthesize #:forall `(,@(list id rest ...) ...)
                   #:init init
                   #:assume assume 
                   #:guarantee spec))]
    
    [(_ ([id rest ... type] ...) #:init init #:guarantee spec)
     (synthesize ([id rest ... type] ...) 
                 #:init init
                 #:assume #t 
                 #:guarantee spec)]
    
    [(_ ([id rest ... type] ...) #:assume assume #:guarantee spec)
     (synthesize ([id rest ... type] ...) 
                 #:init (empty-solution)
                 #:assume assume 
                 #:guarantee spec)]
    
    [(_ ([id rest ... type] ...) #:guarantee spec) 
     (synthesize ([id rest ... type] ...) 
                 #:init (empty-solution)
                 #:assume #t 
                 #:guarantee spec)]
    
    [(_ ([id rest ... type] ...) spec) 
     (synthesize ([id rest ... type] ...) 
                 #:init (empty-solution)
                 #:assume #t 
                 #:guarantee spec)]))

#|--------------helper functions--------------|#

(define always-false (const '(#f)))
(define always-unsat (const '(#f)))

(define (passes? assertion)
  (equal? #t (evaluate assertion (current-solution))))

(define (fails? assertion)
  (false? (evaluate assertion (current-solution))))

