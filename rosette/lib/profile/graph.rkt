#lang racket

(require racket/hash racket/struct
         "data.rkt" "record.rkt" "reporter.rkt" "feature.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Profile graph data structures

;; A profile node is an entry in the dynamic control flow graph of the
;; profiled code. It contains a pointer to its parent node,
;; a list of children nodes, and a profile-data struct that contains the
;; actual data for the profile.
(struct profile-node (id parent children data) #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'profile-node)
      (lambda (obj) (let ([d (profile-node-data obj)])
                      (if d
                          (list (let ([proc (profile-data-procedure d)])
                                  (if (symbol? proc) proc (object-name proc)))
                                (metrics-ref (profile-data-start d) 'time))
                          (list #f #f))))))])


;; Profile data for a single procedure invocation.
;; * The location field stores the location at which the given procedure was
;;   invoked.
;; * The procedure field is the invoked procedure
;; * The inputs and outputs fields are assoc lists from features to numbers.
;;   For each feature in enabled-features, they store the value of that
;;   feature for the inputs and outputs of the current invocation.
;; * The metrics field is a hash map from symbols to numbers, where each
;;   symbol describes a performance metric collected during symbolic evaluation,
;;   e.g., cpu time, real time, gc time, the number of merge invocations, the number
;;   of unions and terms created, etc.
;; * The start and finish fields track the value of various metrics at the entry
;;   and exit to the current invocation, respectively.
(struct profile-data (location procedure inputs outputs metrics start finish) #:mutable #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion

;; Convert an instance of profile-state (i.e., a list of profile events) into a
;; dynamic call graph representation.
(define (profile-state->graph state)
  (define events (reverse (unbox (profile-state-events state))))
  (unless (profile-event-enter? (first events))
    (error 'profile-state->graph "expected first event to be profile-event-enter"))
  (define node #f)
  (define root #f)
  (for ([e events])
    (match e
      [(profile-event-enter met id loc proc in)
       (define new-in (features->flat-hash in))
       (define new-name proc)
       (define new-data (profile-data loc new-name new-in (hash) '() met (hash)))
       (define new-node (profile-node id node '() new-data))
       (if (false? node)
           (let ()
             (set-profile-node-parent! new-node new-node)
             (set! root new-node))
           (set-profile-node-children! node (append (profile-node-children node) (list new-node))))
       (set! node new-node)]
      [(profile-event-exit met out)
       (define data (profile-node-data node))
       (define metrics (diff-metrics (profile-data-start data) met))
       (set-profile-data-metrics! data metrics)
       (define new-out (features->flat-hash out))
       (set-profile-data-outputs! data new-out)
       (set-profile-data-finish!  data met)
       (set! node (profile-node-parent node))]
      [(profile-event-sample met)  ; fill in missing statistics up the callgraph
       (let rec ([node node])
         (define data (profile-node-data node))
         (define metrics (diff-metrics (profile-data-start data) met))
         (set-profile-data-metrics! data metrics)
         (set-profile-data-finish!  data met)
         (unless (eq? node (profile-node-parent node))
          (rec (profile-node-parent node))))]
      [_ void]))
  root)
