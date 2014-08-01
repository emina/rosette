#lang racket

(require (for-syntax racket racket/syntax))

(provide configure configured with-configuration print-configuration)
  
(define configuration% 
  (class* object% (writable<%>) 
    
    (init-field (loop-bound 3)
                (bitwidth 5)
                (seed (vector 2967767310 3507048600 2224288351 795428636 3144875260 3487448621)))
        
    (super-new)
       
    (define/public (set-loop-bound! bound)
      (or (and (>= bound 1)
               (set! loop-bound bound))
          (error 'configure "expects a positive loop bound, given ~s" bound)))
    
    (define/public (set-bitwidth! bound)
      (or (and (>= bound 1) (<= bound 32)
               (set! bitwidth bound))
          (error 'configure "expects a bitwidth between 1 and 32, inclusive, given ~s" bound)))
    
    (define/public (set-seed! vec)
      (or (and (pseudo-random-generator-vector? vec)
               (set! seed vec))
          (error 'configure "expects a pseudo-random-generator-vector?, given ~s" vec)))
      
   
    (define/public (copy)
      (new configuration% 
           [loop-bound loop-bound]
           [bitwidth bitwidth]
           [seed seed]
      ))
    
    (define/public (custom-write port) 
      (fprintf port "[bitwidth ~s]\n" bitwidth)
      (fprintf port "[loop-bound ~s]\n" loop-bound)  
      (fprintf port "[seed ~s]\n" seed)  )
    
    (define/public (custom-display port) (custom-write port))
    ))

(define config (make-parameter (new configuration%)))

(define (print-configuration [port (current-output-port)])
  (fprintf port "~s" (config)))

(define-syntax (configure stx)
  (syntax-case stx ()
    [(_ [field-id value ...]) 
     (let ([setter (format-id #'field-id "set-~a!" (syntax->datum #'field-id))])
       #`(send (config) #,setter value ...))]
    [(_ [field-id value ...] ...)
     #`(begin #,@(map 
                  (lambda (spec) #`(configure #,spec))
                  (syntax->list #'([field-id value ...] ...))))]))
    
(define-syntax (configured stx)
  (syntax-case stx ()
    [(_ [field-id key]) 
     #'(let ([val (get-field field-id (config))])
         (if (hash? val) 
             (hash-ref val key (hash-ref val 'default))  
             (dynamic-send val (quote key))))]  
    [(_ field-id)
     #`(let ([val (get-field field-id (config))])
         (if (hash? val) 
             (hash-ref val 'default)  
             val))]
    [(_ form ...) 
     #`(values #,@(map (lambda (spec) #`(configured #,spec)) 
                       (syntax->list #'(form ...))))]))

(define-syntax (with-configuration stx)
  (syntax-case stx ()
    [(_ (config-spec ...) body ...) 
     #'(parameterize ([config (send (config) copy)])
         (configure config-spec ...)
         body ...)]))

