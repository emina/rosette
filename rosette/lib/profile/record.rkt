#lang racket

(require rosette/base/core/reporter racket/hash
         "data.rkt" "feature.rkt" "reporter.rkt")
(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source location tracking

;; Represents the global map from procedure objects to their source locations (if known).
(define current-sources (make-parameter (make-hash)))

;; Records the mapping from the given procedure object to its source info.
(define (record-source! procs location)
  (for ([p procs])
    (hash-set! (current-sources) p location)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiler run-time state

;; A parameter that holds the current profile / call stack.
(define current-profile (make-parameter #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run-time profile filtering

; Minimum time in milliseconds for a call to be included in the profile.
; Filtering is disabled if this is zero.
(define filtering-threshold (make-parameter 1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run-time profile node updates

;; Records a procedure entry by appending a new profile node to the current one.
;; This procedure should be called after all arguments to the profiled procedure
;; have been evaluated, but before the procedure is invoked.
(define-syntax-rule (record-enter profile reporter)
  (let ([next-id (let ([n 0]) (thunk (begin0 n (set! n (add1 n)))))])
    (lambda (loc proc in)
      (when (and profile (eq? (current-thread) (profile-state-thd profile)))
        (let* ([inputs (compute-features in)]
               [metrics (get-current-metrics/call reporter)]
               [new (profile-event-enter metrics (next-id) loc proc inputs)])
          (profile-state-append! profile new))))))

; Default version just uses the current-profile/current-reporter params
(define default-record-enter
  (record-enter (current-profile) (current-reporter)))

(define record-enter! default-record-enter)


;; Records a procedure exit by modifying the data for the current profile node.
;; This procedure should be called after the profiled procedure call returns.
(define-syntax-rule (record-exit profile reporter threshold)
  (let ([do-record-exit!
         (lambda (out)
           (let* ([outputs (compute-features out)]
                  [metrics (get-current-metrics/call reporter)]
                  [new (profile-event-exit metrics outputs)])
             (profile-state-append! profile new)))])
    (lambda (out)
      (when (and profile (eq? (current-thread) (profile-state-thd profile)))
        (let* ([curr profile]
               [evts (unbox (profile-state-events curr))])
          (cond
            [(or (null? evts)
                 (not (profile-event-enter? (car evts)))
                 (>= (- (current-inexact-milliseconds) (get-call-time (car evts))) threshold))
             (do-record-exit! out)]
            [else  ; ==> (and (< real (filter-threshold)) (profile-event-enter? (car evts)))
             ; prune the previous event, which was an enter
             (define the-box (profile-state-events curr))
             ; if this CAS fails, it can only be because the streaming thread removed
             ; all events from the box. if that happened then the ENTER we're trying
             ; to delete has already been published, and so we need to retain the
             ; corresponding EXIT.
             (unless (box-cas! the-box evts (cdr evts))
               (do-record-exit! out))]))))))

; Default version just uses the current-profile/current-reporter params
(define default-record-exit
  (record-exit (current-profile) (current-reporter) (filtering-threshold)))

(define record-exit! default-record-exit)


; Specialize record-enter! and record-exit!
(define (specialize-recorders! profile reporter threshold)
  (set! record-enter! (record-enter profile reporter))
  (set! record-exit! (record-exit profile reporter threshold)))
; Undo specialization
(define (reset-specialized-recorders!)
  (set! record-enter! default-record-enter)
  (set! record-exit! default-record-exit))


;; Record a sample (used when a profile is unfinished)
(define (record-sample!)
  (profile-state-append! (current-profile) (get-sample-event)))


;; Compute the interesting features of a list of inputs or outputs
(define (compute-features xs)
  (list (cons signature-feature (signature-feature xs))))


;; Records the application of a procedure proc at a location loc to
;; the given by-position and/or keyword arguments.
;; The syntax rule generates the instrumentation for the two different
;; cases (keyword arguments or not), which helps expose some fusion
;; optimization opportunities to the compiler.
(define-syntax-rule (runner app-proc-to-in-expr)
  (let* ([handler  (lambda (exn) (values exn null))]
         [returner (lambda e (values #f e))])
    (lambda (loc proc in)
      (record-enter! loc proc in)
      (call-with-exception-handler
       (lambda (e)
         (record-exit! '())
         e)
       (thunk
        (define ret (call-with-values (lambda () app-proc-to-in-expr) list))
        (record-exit! ret)
        (apply values ret))))))
(define record-apply!
  (make-keyword-procedure
   (lambda (kws kw-args loc proc . rest) 
     ((runner (keyword-apply proc kws kw-args rest)) loc proc (append rest kw-args)))
   (lambda (loc proc . rest)
     ((runner (apply proc rest)) loc proc rest))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level profiler invocation

;; Run a thunk in a given profiler+reporter context. and return its output
(define (run-profile-thunk proc profile reporter)
  (parameterize ([current-profile profile]
                 [current-reporter reporter])
    (specialize-recorders! profile reporter (filtering-threshold))
    (record-enter! #f 'the-profiled-thunk '())
    (define out
      (with-handlers ([exn:break? (lambda (e) 
                                    ((error-display-handler) (if (exn? e) (exn-message e) (~e e)) e)
                                    (record-sample!)
                                    '())])
        (define out (call-with-values proc list))
        (record-exit! out)
        out))
    (reset-specialized-recorders!)
    out))
