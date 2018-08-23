#lang racket

(require "generic.rkt"
         "../../data.rkt" "../../reporter.rkt")
(provide make-solver-calls-component)

(define (make-solver-calls-component options)
  (profile-solver-calls-component))

(struct profile-solver-calls-component () #:transparent
  #:methods gen:report-component
  [(define (init-component self)
     void)
   (define (receive-data self events)
     (list (hash 'type "solver-calls"
                 'events (get-solver-events events))))])

(define (get-solver-events events)
  ; get the solver start and finish events in order
  (define filtered-events
    (filter (lambda (e) (or (profile-event-solve-start? e)
                            (profile-event-solve-finish? e)
                            (profile-event-encode? e)
                            (profile-event-finitize? e)))
            events))

  (for/list ([e filtered-events])
    (let ([time (metrics-ref (profile-event-metrics e) 'time)])
      (match e
        [(profile-event-solve-start _)
         (hash 'part "solver" 'type "start" 'time time)]
        [(profile-event-solve-finish _ sat?)
         (hash 'part "solver" 'type "finish" 'time time 'sat sat?)]
        [(profile-event-encode-start _)
         (hash 'part "encode" 'type "start" 'time time)]
        [(profile-event-encode-finish _)
         (hash 'part "encode" 'type "finish" 'time time)]
        [(profile-event-finitize-start _)
         (hash 'part "finitize" 'type "start" 'time time)]
        [(profile-event-finitize-finish _)
         (hash 'part "finitize" 'type "finish" 'time time)]))))
