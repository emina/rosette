#lang racket

(require "eval.rkt" "state.rkt" 
         (only-in "../base/core/term.rkt" constant? get-type term-cache term<?)
         (only-in "../base/core/equality.rkt" @equal?)
         (only-in "../base/core/bool.rkt" !)
         (only-in "../base/core/real.rkt" @integer? @real?)
         (only-in "../base/core/bitvector.rkt" bv)
         (only-in "../base/core/finitize.rkt" finitize current-bitwidth)
         (only-in "../solver/solver.rkt" send/handle-breaks)
         (only-in "../solver/solution.rkt" model core sat unsat sat? unsat?))

(provide all-true? some-false? unfinitize ∃-solve)

; Returns true if evaluating all given formulas against 
; the provided solution returns the constant #t.
(define (all-true? φs [sol (current-solution)])
  (and (sat? sol) (for/and ([φ φs]) (equal? #t (evaluate φ sol)))))

; Returns true if evaluating at least one of the given 
; formulas against the provided solution returns the constant #f.
(define (some-false? φs [sol (current-solution)])
  (and (sat? sol) (for/or ([φ φs]) (false? (evaluate φ sol)))))

; Takes as input a solution and a finitization map 
; produced by calling the finitize proceudre in rosette/base/core/finitize, 
; and returns a new solution that applies the inverse 
; of the given to the provided solution.
(define (unfinitize sol fmap)
  (match sol
    [(model m)
     (sat (for/hash ([(k fk) fmap] #:when (dict-has-key? m fk))
            (match* ((get-type k) (dict-ref m fk))
              [((or (== @integer?) (== @real?)) (bv v _)) (values k v)]
              [(_ v) (values k v)])))]
    [(core #f) sol] ; no core extracted
    [(core φs)
     (unsat (for/list ([(k v) fmap] #:when (member φs v)) k))]))
     
; Searches for a model, if any, for the conjunction 
; of the given formulas, using the provided solver and 
; bitwidth.  The solver and the bitwidth are, by default, 
; current-solver and current-bitwidth.  Returns an unsat 
; solution if the given formulas don't have a model with 
; the specified bitwidth that is also correct under the 
; precise semantics. Note that this procedure does *not* 
; clear the solver's state either before or after use.
(define (∃-solve φs #:solver [solver (current-solver)] #:bitwidth [bw (current-bitwidth)])
  (cond 
    [bw 
     (parameterize ([term-cache (hash-copy (term-cache))])
       (define fmap (finitize φs bw))
       (send/apply solver assert (for/list ([φ φs]) (hash-ref fmap φ)))
       (let loop ()
         (define fsol (send/handle-breaks solver solve))
         (define sol (unfinitize fsol fmap)) 
         (cond [(or (unsat? sol) (all-true? φs sol)) sol]
               [else (send/apply solver assert 
                        (for/list ([binding (sort (dict->list (model fsol)) term<? #:key car)])
                          (! (@equal? (car binding) (cdr binding)))))
                     (loop)])))]
    [else 
     (send/apply solver assert φs)
     (send/handle-breaks solver solve)]))