#lang racket

(require rosette/query/eval
         rosette/query/state rosette/base/state
         (only-in rosette/base/bool && ! ||)
         (only-in rosette/base/num @<= current-bitwidth)
         (only-in rosette/base/term term? [angelic? angelic?] term-e)
         (only-in rosette/base/assert with-asserts with-asserts-only clear-asserts)
         rosette/config/log 
         rosette/solver/solver 
         rosette/solver/solution
         rosette/solver/kodkod/kodkod
         rosette/solver/z3/z3
         "location.rkt" "instruction.rkt" "fragment.rkt")

(provide ex-all-solve)

(define trial (make-parameter #f))

(define verifier (new kodkod%))
(define synthesizer (new kodkod-incremental%))

;(define verifier (new z3%))
;(define synthesizer (new z3%))

(define (cleanup) 
  (send synthesizer shutdown)
  (send verifier shutdown))
  
; A wrapper around the ExAllSolver from Gulwani et al. "Synthesis of Loop-Free Programs", PLDI'11.
; The wrapper synthesizes programs using successively larger bitwidths until it produces one that 
; works on inputs of size (configured bitwidth).  It takes as input the list of arguments accepted  
; by the synthesized procedure, pre/post condition predicates, a list of library 
; components, and the minimum starting bitwidth.  Its output, if any, is stored in the 
; (current-solution) parameter.
(define (ex-all-solve inputs pre post lib min-bw)
  (define bw (current-bitwidth))
  (define lbw (location-bitwidth inputs lib))
  (unless (<= lbw bw)
    (error 'synthesize "expected a bitwidth of at least ~a, given ~a" lbw bw))
  (parameterize ([trial 0])
    (let loop ([pbw (max lbw (min min-bw bw))])
      (with-handlers ([exn:fail? (lambda (e) (log-info "~a" e) (if (>= pbw bw) (begin (cleanup) (raise e)) (loop (+ pbw 1))))])
        (log-info "synthesizing a ~a-bit fragment ..." pbw) 
        (parameterize ([current-oracle (oracle)] 
                       [current-bitwidth pbw]) 
          (ex-all-solve* inputs pre post (copy-library #:library lib)))                                                      
        (send verifier clear)
        (global-verification-constraint verifier (current-solution) inputs pre post lib)  
        (define cex (solve/handle-breaks verifier cleanup))
        (send verifier clear)
        (cond [(sat? cex) 
               (cond [(< pbw bw) 
                      (log-info "fragment incorrect on ~a-bit inputs ~a; increasing bitwidth ..." bw (map cex inputs))
                      (loop (+ 1 pbw))]
                     [else 
                      (cleanup)
                      (error 'synthesize "fragment incorrect on ~a-bit inputs ~a; no solution found." bw (map cex inputs))])]
              [else           
               (log-info "fragment verified on ~a-bit inputs!" bw)
               (cleanup)])))))

; Implements the ExAllSolver from Gulwani et al. "Synthesis of Loop-Free Programs", PLDI'11.
; The solver takes as input a list of inputs, pre/post condition predicates and a list of library 
; components.  Its output is stored in the (current-solution) parameter.
(define (ex-all-solve* inputs pre post lib)
  (send verifier clear)
  (send synthesizer clear)
  (clear-asserts)
  
  (log-cegis-info "searching for an initial solution ...")

  (initialize-synthesizer-alt synthesizer cleanup inputs pre post lib)
  
  (let loop ([sol (solve/unbind synthesizer angelic? cleanup)])
    
    ;(log-info "CANDIDATE SOLUTION ~a" (syntax->datum (fragment->syntax #:solution sol #:inputs (map term-e inputs) #:library lib)))
   
    (verification-constraint verifier sol inputs pre post lib)  
    (define cex (solve/handle-breaks verifier cleanup))
    (send verifier clear)

    (if (sat? cex)        
        (begin
          (log-cegis-info "solution falsified by ~a; searching for a new candidate ..." (map cex inputs))
          (synthesis-constraint synthesizer (map cex inputs) pre post lib)
          (trial (+ (trial) 1))
          (loop (solve/unbind synthesizer angelic? cleanup)))
        (begin            
          (log-cegis-info "solution verified!")
          (cleanup)
          (current-solution sol)))))

(define (solve/handle-breaks solver cleanup)
  (with-handlers ([exn:break? (lambda (e) (cleanup) (raise e))])
    (send solver solve)))

(define (solve/unbind synthesizer unbind? cleanup)
  (let ([sol (solve/handle-breaks synthesizer cleanup)])
    (unless (sat? sol)
      (cleanup)
      (error 'synthesize "synthesis failed"))
    (unbind sol unbind?)))

(define (verification-constraint verifier  sol inputs pre post lib)
  (let* ([F_wfp  (with-asserts-only (check/wfp #:inputs inputs #:library lib))]
         [F_conn (with-asserts-only (check/conn #:inputs inputs #:library lib))]
         [F_insts (with-asserts-only (check/pre #:library lib))]
         [F_spec (with-asserts-only (check/spec #:inputs inputs #:spec post #:library lib))])
    ;(log-info "VERIFY: (PRE inputs) = ~a" (apply pre inputs))
    ;(log-info "VERIFY: F_wfp = ~a" (remove* '(#t) (map (curryr evaluate sol) F_wfp)))
    ;(log-info "VERIFY: F_conn = ~a" (remove* '(#t) (map (curryr evaluate sol) F_conn)))
    ;(log-info "VERIFY: F_insts = ~a" (remove* '(#t) (map (curryr evaluate sol) F_insts)))
    ;(log-info "VERIFY: !F_spec = ~a" (evaluate (apply || (map ! F_spec)) sol))
    (send verifier assert (apply pre inputs))
    (for ([assumption (in-sequences F_wfp F_conn)])
      (send verifier assert (evaluate assumption sol)))
    (send verifier assert (evaluate (apply || (map ! (append F_insts F_spec))) sol))))

(define (synthesis-constraint synthesizer inputs pre post lib)
  ; We handle synthesis different than verification.  In particular,
  ; we have to make sure that fresh temporary variables are generated 
  ; for each instruction's input/output values.  We do this by simply 
  ; using a fresh copy of the library to construct the synthesis constraint.
  ; Since instruction input/output values are angelic variables, copying 
  ; the library will automatically ensure that we get fresh temporary 
  ; variables representing input/output values. 
  (let* ([lib (copy-library #:library lib)]
         [F_conn (with-asserts-only (check/conn #:inputs inputs #:library lib))]
         [F_insts (with-asserts-only (check/pre #:library lib))]
         [F_spec (with-asserts-only (check/spec #:inputs inputs #:spec post #:library lib))]
         [F_pre (apply pre inputs)])
    
    ;(log-info "SYNTH: inputs = ~a" inputs)
    ;(log-info "SYNTH: F_pre = ~a" F_pre)
    ;(log-info "SYNTH: F_conn = ~a" F_conn)
    ;(log-info "SYNTH: F_insts = ~a"  F_insts)
    ;(log-info "SYNTH: F_spec = ~a"  F_spec)
    (send synthesizer assert F_pre)
    (send/apply synthesizer assert F_conn)
    (send/apply synthesizer assert F_insts)
    (send/apply synthesizer assert F_spec)))

; Creates a copy of the given library such that each copied 
; instruction gets fresh instruction-in-valeus variables, but 
; it shares the instruction procedure, id, input and output locations
; with the original instruction. 
(define (copy-library #:library lib)
  (for/list ([inst lib])
    (new-instruction (instruction-id inst)
                     (instruction-proc inst)
                     (instruction-inputs inst)
                     (instruction-output inst))))

(define-syntax-rule (log-cegis-info msg rest ...) 
  (log-info "[~a] ~a" (trial) (format msg rest ...)))

(define (global-verification-constraint verifier sol inputs pre post lib)
  (let*-values ([(fragment) (apply vector (append inputs (fragment #:library lib #:solution sol)))]
                [(out asserts) (with-asserts (interpret sol fragment))])    
    ;(log-info "asserts = ~a" asserts)
    ;(log-info "out = ~a" out)
    ;(log-info "FRAGMENT: ~a" fragment)
    (send verifier assert (apply pre inputs))
    (send verifier assert (apply || (map ! (cons (apply post `(,@inputs ,out)) asserts))))))

(define (interpret sol fragment)
  (define (fragment-ref idx) (vector-ref fragment idx))
  (for ([(inst idx) (in-indexed fragment)] #:when (instruction? inst))
    (vector-set! 
     fragment idx 
     (evaluate
      (apply (instruction-proc inst)
             (map (compose1 fragment-ref location-ordinal sol) (instruction-inputs inst)))
      sol)))
  (fragment-ref (- (vector-length fragment) 1)))

(define (initialize-synthesizer-alt synthesizer cleanup inputs pre post lib)
  (let* ([F_sbp (with-asserts-only (assert/sbp #:library lib))]
         [F_wfp (with-asserts-only (check/wfp #:inputs inputs #:library lib))])
    ;(log-info "SYNTH: F_sbp = ~a" F_sbp)
    ;(log-info "SYNTH: F_wfp = ~a" F_wfp)
    (send/apply synthesizer assert F_sbp)
    (send/apply synthesizer assert F_wfp))
  (synthesis-constraint synthesizer inputs pre post lib))

(define (initialize-synthesizer synthesizer cleanup inputs pre post lib)
  (send/apply synthesizer assert (with-asserts-only (assert/sbp #:library lib)))
  (send/apply synthesizer assert (with-asserts-only (check/wfp #:inputs inputs #:library lib)))
  (synthesis-constraint synthesizer inputs pre post lib)
  (define init-sol (solve/unbind synthesizer none/c cleanup))
  (send synthesizer clear)
  (send/apply synthesizer assert (with-asserts-only (assert/sbp #:library lib)))
  (send/apply synthesizer assert (with-asserts-only (check/wfp #:inputs inputs #:library lib)))
  (synthesis-constraint synthesizer (map init-sol inputs) pre post lib))


          
  
