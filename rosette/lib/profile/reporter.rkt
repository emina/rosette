#lang racket

(require rosette/base/core/reporter "data.rkt")
(provide (struct-out profiler-reporter) make-profiler-reporter
         get-current-metrics/call get-call-time
         get-sample-event
         metrics-ref diff-metrics metrics->hash)

; The profiler reporter keeps a cumulative count of several metrics,
; as an association list, and reports
; them when requested to insert into a profile node.
; (Performance note: an association list is slightly faster than a hash table
; for workloads that clone the current metrics state a lot, such as MemSynth).
(define (make-profiler-reporter profile)
  (profiler-reporter
   profile
   (map (curryr cons 0) '(term-count merge-count merge-cases union-count union-size))
   #f))

(struct profiler-reporter (profile [metrics #:mutable] [finitizing #:mutable])
  #:transparent
  #:property prop:procedure
  (lambda (self . rest)
    (match rest
      [(list 'new-term the-term)
       (unless (profiler-reporter-finitizing self)
         (inc! self 'term-count 1))
       (let* ([new (profile-event-term-new (get-current-metrics/none) the-term)])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [(list 'merge merge-cases)
       (unless (profiler-reporter-finitizing self)
         (inc! self 'merge-count 1)
         (inc! self 'merge-cases merge-cases))]
      [(list 'new-union union-size)
       (unless (profiler-reporter-finitizing self)
         (inc! self 'union-count 1)
         (inc! self 'union-size union-size))]
      [(list 'solve-start)
       (let* ([new (profile-event-solve-start (get-current-metrics/event))])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [(list 'solve-finish sat?)
       (let* ([new (profile-event-solve-finish (get-current-metrics/event) sat?)])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [(list 'to-solver lists ...)
       (let* ([new (profile-event-solve-encode (get-current-metrics/none) lists)])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [(list 'finitize-start)
       (set-profiler-reporter-finitizing! self #t)
       (let* ([new (profile-event-finitize-start (get-current-metrics/event))])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [(list 'finitize-finish)
       (set-profiler-reporter-finitizing! self #f)
       (let* ([new (profile-event-finitize-finish (get-current-metrics/event))])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [(list 'encode-start)
       (let* ([new (profile-event-encode-start (get-current-metrics/event))])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [(list 'encode-finish)
       (let* ([new (profile-event-encode-finish (get-current-metrics/event))])
         (profile-state-append! (profiler-reporter-profile self) new))]
      [_ (void)])))


(define (assoc-inc xs x v)
  (let loop ([xs xs])
    (cond [(null? xs) (cons x v)]
          [(eq? (caar xs) x) (cons (cons x (+ v (cdar xs))) (cdr xs))]
          [else (cons (car xs) (loop (cdr xs)))])))
(define (assoc-dec xs x v)
  (let loop ([xs xs])
    (cond [(null? xs) (cons x v)]
          [(eq? (caar xs) x) (cons (cons x (- (cdar xs) v)) (cdr xs))]
          [else (cons (car xs) (loop (cdr xs)))])))

(define-syntax-rule (inc! reporter key val)
  (let ([ht (profiler-reporter-metrics reporter)])
    (set-profiler-reporter-metrics! reporter (assoc-inc ht key val))))
(define-syntax-rule (dec! reporter key val)
  (let ([ht (profiler-reporter-metrics reporter)])
    (set-profiler-reporter-metrics! reporter (assoc-dec ht key val))))


(define (get-current-metrics/event)
  (list (cons 'time (current-inexact-milliseconds))))
(define (get-current-metrics/none)
  '())
(define (get-current-metrics/call reporter)
  (cons (cons 'time (current-inexact-milliseconds))
        (profiler-reporter-metrics reporter)))
; shortcut to get time from a get-current-metrics/call instance;
; make sure to update if get-current-metrics/call changes
(define (get-call-time evt)
  (cdar (profile-event-metrics evt)))


(define (get-sample-event)
  (profile-event-sample (get-current-metrics/call (current-reporter))))


;; Abstract out references to metrics in case we decide we need a better data
;; structure at some point.
(define (metrics-ref mets key)
  (let ([a (assq key mets)])
    (if a (cdr a) #f)))


;; Helper to compute the difference between entry and exit metrics
(define (diff-metrics old new)
  (for/list ([k/v new])
    (let ([k (car k/v)][v (cdr k/v)])
      (let ([o (assq k old)])
        (cons k (- v (if o (cdr o) 0)))))))

;; Convert metrics to a hash for output
(define (metrics->hash m)
  (for/hash ([k/v m]) (values (car k/v) (cdr k/v))))
