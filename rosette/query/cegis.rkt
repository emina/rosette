#lang racket

(require "eval.rkt" 
         "state.rkt" 
         (only-in "../base/core/term.rkt" constant) 
         (only-in "../base/core/bool.rkt" @boolean? ! ||) 
         (only-in "../base/core/num.rkt" @number? ignore-division-by-0)
         (only-in "../base/struct/enum.rkt" enum? enum-first)
         "../base/util/log.rkt"
         "../solver/solver.rkt"  
         "../solver/solution.rkt"
         "../solver/smt/z3.rkt")

(provide exists-forall)

; Solves the exists-forall problem for the given list of inputs, 
; assumptions, assertions, and initial binding(s) of inputs
; to concrete values.   The initial binding may be given as a single 
; model mapping inputs to values, or a list of models mapping inputs to values.
; If I is the set of all input variables, and H is the set of the remaining 
; (non-input) variables appearing in the assumptions and the assertions. 
; This algorithm solves the following constraint: \exists H \forall I . assumes => asserts
(define (exists-forall inputs assumes asserts inits)
  
  (set! inits (check-initial-bindings inputs (if (list? inits) inits (list inits))))

  (define-values (synthesizer verifier) (values (new z3%) (new z3%)))
  
  (define (cleanup) 
    (send synthesizer shutdown)
    (send verifier shutdown))
  
  (define (input? var) (member var inputs))
  
  (log-cegis-info [0] "searching for an initial solution over ~s" (map (curryr map inputs) inits))
  (parameterize ([ignore-division-by-0 #t])
    (for ([init inits])
      (cond [(zero? (dict-count (model init)))
             (send/apply synthesizer assert assumes)
             (send/apply synthesizer assert asserts)]
            [else 
             (send/apply synthesizer assert (evaluate assumes init))
             (send/apply synthesizer assert (evaluate asserts init))])))
  
  ;(log-cegis-info [0] "assumes: ~a\n" (pretty-format assumes))
  ;(log-cegis-info [0] "asserts: ~a\n" (pretty-format asserts))
  
  (let loop ([sol (solve/clean synthesizer input? cleanup)] [trial 0])
    
    (unless (sat? sol)
      (cleanup)
      (error 'synthesize "failed to find a candidate solution"))
    
    (log-cegis-info [trial] "verifying the candidate solution ...")
    
    (send/apply verifier assert (evaluate assumes sol))
    (send verifier assert (negate asserts sol))
    
    (define cex (send/handle-breaks verifier solve cleanup)) 
    (send verifier clear)
    
    (if (sat? cex)        
        (let ([witness (cex->witness cex inputs)])
          (log-cegis-info [trial] "solution falsified by ~s; searching for a new candidate ..." (map witness inputs))
          (parameterize ([ignore-division-by-0 #t])
            (send/apply synthesizer assert (evaluate assumes witness))
            (send/apply synthesizer assert (evaluate asserts witness)))
          (loop (solve/unbind synthesizer input? cleanup) (+ 1 trial)))
        (begin            
          (log-cegis-info [trial] "solution verified!")
          (cleanup)
          (current-solution sol)))))

(define (check-initial-bindings inputs inits)
  (for ([init inits])
    (unless (and (solution? init) (sat? init))
      (error 'synthesize "expected satisfiable initial binding(s) from ~s to values, given ~s" inputs init))
    (let* ([bindings (model init)]
           [size (dict-count bindings)])
      (or (zero? size)
          (= size (for/sum ([input inputs] #:when (dict-has-key? bindings input)) 1))
          (error 'synthesize "expected only bindings for ~s, given ~s" inputs init))))
  inits)
   
(define (negate asserts sol)
  (apply || (map ! (evaluate asserts sol)))) 

(define (solve/unbind synthesizer unbind? cleanup)
  (let ([sol (send/handle-breaks synthesizer solve cleanup)])
    (unless (sat? sol)
      (cleanup)
      (error 'synthesize "synthesis failed"))
    (unbind sol unbind?)))

(define-syntax-rule (log-cegis-info [trial] msg rest ...) 
  (log-info ['cegis] "[~a] ~a" trial (format msg rest ...)))
                                          
(define (solve/clean  synthesizer input? cleanup)
  (begin0 
    (solve/unbind synthesizer input? cleanup)
    (send synthesizer clear)))

(define (cex->witness cex inputs)  
  (sat (for/hash ([in inputs]) 
         (match (cex in)
           [(constant _ (== @number?))
            (values in 0)]
           [(constant _ (== @boolean?))
            (values in #f)]
           [(constant _ (? enum? t))
            (values in (enum-first t))]
           [val (values in val)]))))   
