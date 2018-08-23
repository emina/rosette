#lang racket

(require "generic.rkt"
         "../../data.rkt" "../../reporter.rkt" "../../feature.rkt" "../../record.rkt"
         "../syntax.rkt")
(provide make-callgraph-component)

; The callgraph component simply passes through to the client all the events
; needed to construct the call graph.

(define (make-callgraph-component options)
  (callgraph-report-component))

(struct callgraph-report-component () #:transparent
  #:methods gen:report-component
  [(define (init-component self)
     void)
   (define (receive-data self events)
     (list (hash 'type "callgraph"
                 'events (events->jsexpr events))))])

(define (render-event? event)
  (or (profile-event-enter? event)
      (profile-event-exit? event)
      (profile-event-sample? event)))


(define (render-event event)
  (match event
    [(profile-event-enter met id loc proc in)
     (hash 'type "ENTER"
           'id id
           'function (procedure-name proc)
           'callsite (syntax-srcloc loc)
           'source (let ([src (hash-ref (current-sources) proc #f)]) (if src (syntax-srcloc src) src))
           'inputs (features->flat-hash in)
           'metrics (metrics->hash met))]
    [(profile-event-exit met out)
     (hash 'type "EXIT"
           'outputs (features->flat-hash out)
           'metrics (metrics->hash met))]
    [(profile-event-sample met)
     (hash 'type "SAMPLE"
           'metrics (metrics->hash met))]
    [_ (error 'render-event "unknown event ~v" event)]))


(define (events->jsexpr events)
  (map render-event (filter render-event? events)))
