#lang racket

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profiler data structures

;; A profile consists of a mutable box events, which contains a list of
;; profile-event? records.
(struct profile-state (events thd))

(struct profile-event (metrics))
(struct profile-event-enter    profile-event     (id location procedure inputs) #:transparent)
(struct profile-event-exit     profile-event     (outputs))
(struct profile-event-sample   profile-event     ())
(struct profile-event-pc       profile-event     ())
(struct profile-event-solve    profile-event     ())
(struct profile-event-solve-start  profile-event-solve ())
(struct profile-event-solve-finish profile-event-solve (sat?))
(struct profile-event-solve-encode profile-event-solve (asserts))
(struct profile-event-finitize profile-event     ())
(struct profile-event-finitize-start profile-event-finitize ())
(struct profile-event-finitize-finish profile-event-finitize ())
(struct profile-event-encode profile-event     ())
(struct profile-event-encode-start profile-event-encode ())
(struct profile-event-encode-finish profile-event-encode ())
(struct profile-event-term     profile-event     (val))
(struct profile-event-term-new profile-event-term ())

;; Returns a new profile
(define (make-profile-state)
  (profile-state (box '()) (current-thread)))

;; Append a new event to a profile state
(define (profile-state-append! state evt)
  (define the-box (profile-state-events state))
  (let loop ()
    (let* ([old (unbox the-box)]
           [new (cons evt old)])
      (unless (box-cas! the-box old new)
        (loop)))))
