#lang racket

(require 
  "eval.rkt" "finitize.rkt"
  (only-in "../base/core/term.rkt" constant? term-type get-type term? with-terms clear-terms! term<? solvable-default)
  (only-in "../base/core/bool.rkt" ! || && => @boolean?)
  "../solver/solver.rkt"
  (only-in "../solver/solution.rkt" model core sat unsat sat? unsat?)
  (only-in "../solver/smt/z3.rkt" z3))

(provide current-solver ∃-solve ∃-solve+ ∃∀-solve)

; Current solver instance that is used for queries and kept alive for performance.
(define current-solver
  (make-parameter (z3)
                  (lambda (s)
                    (unless (solver? s)
                      (error 'current-solver "expected a solver?, given ~s" s))
                    (solver-shutdown (current-solver))
                    s)))

; Searches for a model, if any, for the conjunction 
; of the given formulas, using the provided solver and 
; bitwidth.  The solver and the bitwidth are, by default, 
; current-solver and current-bitwidth. This procedure 
; clears the solver's state before and after use.
(define (∃-solve φs
                 #:minimize [mins '()]
                 #:maximize [maxs '()]
                 #:solver [solver (current-solver)]
                 #:bitwidth [bw (current-bitwidth)])
  (solver-clear solver)
  (begin0  
    (with-handlers ([exn? (lambda (e) (solver-shutdown solver) (raise e))])
      (cond 
        [bw 
         (with-terms
           (let ([fmap (finitize (append φs mins maxs) bw)])
             (solver-assert solver (for/list ([φ φs]) (hash-ref fmap φ)))
             (solver-minimize solver (for/list ([m mins]) (hash-ref fmap m)))
             (solver-maximize solver (for/list ([m maxs]) (hash-ref fmap m)))
             (unfinitize (solver-check solver) fmap)))]
        [else 
         (solver-assert solver φs)
         (solver-minimize solver mins)
         (solver-maximize solver maxs)
         (solver-check solver)]))
    (solver-clear solver)))

; Returns a stateful procedure that uses the solver of the given type, with the given 
; bitwidth setting, to incrementally solve a sequence of constraints.  The procedure   
; consumes a constraint (i.e., a boolean value or term), a positive integer, or
; the symbol 'shutdown.
; If the argument is a constraint, it is pushed onto the current assertion stack and
; a solution for all constraints on the stack is returned.
; If it the argument is a positive integer k, then the top k constraints are popped
; from the stack and the result is the solution to the remaining constraints.
; If the argument is 'shutdown, all resources used by the procedure are released, and any
; subsequent calls to the procedure throw an exception. 
(define (∃-solve+ #:solver [solver-type #f] #:bitwidth [bw (current-bitwidth)])
  (define cust (make-custodian))
  (define solver
    (parameterize ([current-custodian cust]
                   [current-subprocess-custodian-mode 'kill])
      (if (false? solver-type) ((solver-constructor (current-solver))) (solver-type))))
  (define handler
    (lambda (e)
      (when (and solver cust)
        (solver-shutdown solver)
        (custodian-shutdown-all cust)
        (set! solver #f)
        (set! cust #f))
      (raise e)))
  (define sols (list (sat)))
  (if bw
      (let ([fmap (make-hash)])
        (lambda (δ)
          (with-handlers ([exn? handler])            
            (cond [(or (boolean? δ) (term? δ))
                   (finitize (list δ) bw fmap)
                   (solver-push solver)
                   (solver-assert solver (list (hash-ref fmap δ)))
                   (define sol (unfinitize (solver-check solver) fmap))
                   (set! sols (cons sol sols))
                   sol]
                  [(equal? δ 'shutdown)
                   (solver-shutdown solver) 
                   (custodian-shutdown-all cust)
                   (set! solver #f)
                   (set! cust #f)
                   (clear-terms! ; Purge finitization terms from the cache
                    (for/list ([(t ft) fmap] #:when (and (term? ft) (not (eq? t ft)))) ft))]
                  [else
                   (solver-pop solver δ)
                   (set! sols (drop sols δ))
                   (car sols)])))) 
      (lambda (δ)
        (with-handlers ([exn? handler])
          (cond [(or (boolean? δ) (term? δ))
                 (solver-push solver)
                 (solver-assert solver (list δ))
                 (define sol (solver-check solver))
                 (set! sols (cons sol sols))
                 sol]
                [(equal? δ 'shutdown)
                 (solver-shutdown solver)
                 (custodian-shutdown-all cust)
                 (set! solver #f)
                 (set! cust #f)]
                [else
                 (solver-pop solver δ)
                 (set! sols (drop sols δ))
                 (car sols)])))))
                 

; Solves the exists-forall problem for the provided list of inputs, assumptions and assertions. 
; That is, if I is the set of all input symbolic constants, 
; and H is the set of the remaining (non-input) constants appearing 
; in the assumptions and the assertions, 
; this procedure solves the following constraint: 
; ∃H . ∀I . assumes => asserts.
; Note, however, that the procedure will *not* produce models that satisfy the above 
; formula by making assumes evaluate to false.
(define (∃∀-solve inputs assumes asserts #:solver [solver #f] #:bitwidth [bw (current-bitwidth)])
  (define solver-type (if (false? solver) (solver-constructor (current-solver)) solver))
  (parameterize ([current-custodian (make-custodian)]
                 [current-subprocess-custodian-mode 'kill])
    (with-terms
      (with-handlers ([exn? (lambda (e) (custodian-shutdown-all (current-custodian)) (raise e))])
        (begin0 
          (cond 
            [bw
             (define fmap (finitize (append inputs assumes asserts) bw))
             (define fsol (cegis (for/list ([i inputs])  (hash-ref fmap i))
                                 (for/list ([φ assumes]) (hash-ref fmap φ))
                                 (for/list ([φ asserts]) (hash-ref fmap φ))
                                 (solver-type) (solver-type)))
             (unfinitize fsol fmap)]
            [else 
             (cegis inputs assumes asserts (solver-type) (solver-type))])
          (custodian-shutdown-all (current-custodian)))))))
         

; Uses the given solvers to solve the exists-forall problem 
; for the provided list of inputs, assumptions and assertions. 
; That is, if I is the set of all input symbolic constants, 
; and H is the set of the remaining (non-input) constants appearing 
; in the assumptions and the assertions, 
; this procedure solves the following constraint: 
; ∃H. (∀I. assumes => asserts) ∧ (∃I. assumes).
(define (cegis inputs assumes asserts guesser checker)
  
  (define φ   (append assumes asserts))
  
  (define ¬φ `(,@assumes ,(apply || (map ! asserts))))
  
  (define (guess sol)
    (solver-assert guesser (evaluate φ sol))
    (match (solver-check guesser)
      [(model m) (sat (for/hash ([(c v) m] #:unless (member c inputs)) (values c v)))]
      [other other]))
  
  (define (check sol)
    (solver-clear checker)
    (solver-assert checker (evaluate ¬φ sol))
    (match (solver-check checker)
      [(? sat? m) (sat (for/hash ([i inputs])
                         (values i (let ([v (m i)])
                                     (if (eq? v i)
                                         (solvable-default (term-type i))
                                         v)))))]
      [other other]))
    
  (let loop ([candidate (begin0 (guess (sat)) (solver-clear guesser))])
    (cond 
      [(unsat? candidate) candidate]
      [else
        (let ([cex (check candidate)])
          (cond 
            [(unsat? cex) candidate]
            [else (loop (guess cex))]))])))


        
