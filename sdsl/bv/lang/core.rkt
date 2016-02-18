#lang racket

(require 
  (only-in "bvops.rkt" BV bv* bv)
  (only-in "program.rkt" trace* trace-args trace-out well-formed-program well-formed-trace)
  rosette/query/eval
  (only-in rosette/base/core/term term? constant? get-type term-cache term<?)
  (only-in rosette/base/core/bool ! || && => with-asserts-only)
  (only-in rosette/base/core/real @integer?)
  (only-in rosette/base/core/bitvector bitvector bitvector-size)
  (only-in rosette/base/util/log log-info)
  (only-in rosette/solver/solution model sat sat? unsat? unsat)
  (only-in rosette/solver/smt/z3 z3)
  (only-in rosette/solver/solver assert solve clear))

(provide ∃∀-solve)

; A wrapper around the ∃∀ solver from Gulwani et al. "Synthesis of Loop-Free Programs", PLDI'11.
; The wrapper synthesizes programs using successively larger bitwidths until it produces one that 
; works on inputs of size (bitvector-size (BV)).  It takes as input 
; * a symbolic program,
; * a functional correctness specification (in the form of a reference implementation), and 
; * the minimum starting bitwidth.  
; It produces a solution (if one exists) that can be used to construct a loop-free program that 
; satisfies the given specification.
(define (∃∀-solve impl spec minbw)
  (parameterize ([current-custodian (make-custodian)]
                 [current-subprocess-custodian-mode 'kill])
    (with-handlers ([exn? (lambda (e) (custodian-shutdown-all (current-custodian)) (raise e))])
      (define guesser (z3))
      (define checker (z3))
      (define maxbw (bitvector-size (BV)))
      (define (correct? sol)
        (and (sat? sol)
             (parameterize ([term-cache (hash-copy (term-cache))]
                            [BV (bitvector maxbw)])
               (clear checker)
               (assert checker (φ_verify (evaluate (trace* impl) sol) spec))
               (unsat? (solve checker)))))
      (let loop ([bw (min (max 1 minbw) maxbw)])
        (define candidate 
          (parameterize ([term-cache (hash-copy (term-cache))]
                         [BV (bitvector bw)])
            (∃∀-solve* impl spec guesser checker)))
        (cond [(or (= bw maxbw) (correct? candidate))
               (custodian-shutdown-all (current-custodian))
               candidate]
              [else (loop (add1 bw))])))))
     
         
; Implements the ∃∀ solver from Gulwani et al. "Synthesis of Loop-Free Programs", PLDI'11.
; The procedure takes as input a symbolic program and a functional correctness specification 
; of its desired behavior, in the form of a reference implementation), as well as 
; guesser and checker solver instances.
; It produces a solution (if one exists) that can be used to construct a loop-free program that 
; satisfies the given specification.
(define (∃∀-solve* impl spec guesser checker)
  
  (define trial 0)
  
  (define t0 (trace* impl))        ; initial symbolic trace
  (define inputs (trace-args t0))  ; symbolic inputs
  
  (define (init)
    (clear guesser)
    (assert guesser (φ_wfp impl))
    (assert guesser (φ_synth t0 spec))
    (log-solver-info [trial] "searching for an initial candidate at ~a" (BV))
    (begin0
      (solution->candidate (solve guesser))
      (clear guesser)
      (assert guesser (φ_wfp impl))))
    
  (define (guess sol)
    (set! trial (add1 trial))
    (define sample (map sol inputs))
    (log-solver-info [trial] "searching for a candidate: ~s" sample)
    (assert guesser (φ_synth (trace* impl sample) spec))
    (solution->candidate (solve guesser)))
  
  (define (check sol)
    (clear checker)
    (assert checker (φ_verify (evaluate t0 sol) spec))
    (solution->sample (solve checker) inputs))
    
  (let loop ([candidate (init)])
    (cond 
      [(unsat? candidate) candidate]
      [else
        (let ([cex (check candidate)])
          (cond 
            [(unsat? cex) candidate]
            [else (loop (guess cex))]))])))
     
(define (φ_synth trace spec) ;(printf "φ_synth: ~a\n" trace)
  (append (φ_wft trace) (φ_spec trace spec)))

(define (φ_verify trace spec) ;(printf "φ_verify: ~a\n" trace)
  `(,@(φ_wft trace) ,(apply || (map ! (φ_spec trace spec)))))

(define (φ_wfp impl) ;(printf "φ_wfp: ~a\n" impl)
  (with-asserts-only (well-formed-program impl)))

(define (φ_wft trace) 
  (with-asserts-only (well-formed-trace trace)))

(define (φ_spec trace spec)
  (with-asserts-only (trace-out trace (apply spec (trace-args trace)))))

(define (solution->candidate sol)
  (match sol
    [(model m) 
     (sat (for/hash ([(c v) m] #:when (equal? @integer? (get-type c))) 
            (values c v)))]
    [_ sol]))

(define (solution->sample sol inputs)
  (match sol
    [(model m) 
     (sat (for/hash ([i inputs]) 
            (values i (dict-ref m i (bv 0)))))]
    [_ sol]))
 
(define-syntax-rule (log-solver-info [trial] msg rest ...) 
  (log-info "[~a] ~a" trial (format msg rest ...)))   


#|
(require "program.rkt")
;(current-log-handler (log-handler #:info any/c))
(define impl (prog* 1 (list (op #'identity identity))))
(∃∀-solve impl identity 4)
|#