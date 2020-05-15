#lang racket

(require racket/date racket/runtime-path racket/async-channel net/sendurl json
         "../data.rkt" "../reporter.rkt" (only-in "../record.rkt" filtering-threshold)
         "renderer.rkt" "syntax.rkt"
         "report/generic.rkt"
         "report/callgraph.rkt" "report/solver.rkt" "report/terms.rkt"
         "../../util/streaming-server.rkt")
(provide make-report-renderer make-report-stream-renderer)

; The report renderer produces HTML output by sending
; profile events to a collection of report components,
; which each generate JSON-formatted messages to be passed
; to the client webpage (either by streaming or statically).


; The path containing the HTML template
(define-runtime-path template-dir "report/html")


; Components that will be generating messages for this profile
(define report-components
  (list make-callgraph-component
        make-solver-calls-component
        make-terms-component))


(struct report-options (symlink? interval) #:transparent)


; Make a non-streaming report renderer
(define (make-report-renderer source name [options (hash)])
  (define components (for/list ([c report-components]) (c options)))
  (report-renderer source name components (report-options (hash-ref options 'symlink #f)
                                                          (hash-ref options 'interval 2.0))))

; Make a streaming report renderer
(define (make-report-stream-renderer source name [options (hash)])
  (define components (for/list ([c report-components]) (c options)))
  (report-renderer/stream source name components
                          (report-options (hash-ref options 'symlink #f)
                                          (hash-ref options 'interval 2.0))
                          #f #f))


(struct report-renderer (source name components options)
  #:transparent
  #:methods gen:renderer
  [(define (start-renderer self profile reporter)
     (for ([c (report-renderer-components self)])
       (init-component c)))
   (define (finish-renderer self profile)
     (match-define (report-renderer source name components options) self)
     (define unpruned-events (reverse (unbox (profile-state-events profile))))
     (define events (if (null? unpruned-events)
                        unpruned-events
                        (prune-short-events unpruned-events)))
     (define messages 
       (cons (metadata-message source name)
             (apply append (for/list ([c components]) (receive-data c events)))))
     (define path (render-html-template source (report-options-symlink? options)))
     (render-report-messages messages path)
     (open-report-in-browser path))])


(struct report-renderer/stream report-renderer ([shutdown! #:mutable] [channel #:mutable])
  #:transparent
  #:methods gen:renderer
  [(define (start-renderer self profile reporter)
     (match-define (report-renderer/stream source name components options _ _) self)
     (for ([c (report-renderer-components self)])
       (init-component c))
     ; launch the server


     (define events-box (profile-state-events profile))

     (define-values (port shutdown! connected-channel)
       (start-streaming-server
        (thunk
         (define events
           (reverse
            (cons (profile-event-sample (get-current-metrics/call reporter))
                  (unbox/replace! events-box '()))))
         (define filtered-events (if (null? events) events (prune-short-events events)))
         ; get the messages from each component
         (apply append (for/list ([c components]) (receive-data c filtered-events))))
        (report-options-interval options)
        stream-finish-message))
     (set-report-renderer/stream-shutdown!! self shutdown!)
     (set-report-renderer/stream-channel! self connected-channel)
     ; open the browser with the initial messages
     (define path (render-html-template source (report-options-symlink? options)))
     (define messages
       (list (metadata-message source name)
             (stream-start-message port)))
     (render-report-messages messages path)
     (open-report-in-browser path)
     ; wait for the browser to open the connection
     (match (channel-get connected-channel)
       ['connected (void)]
       [x (error "unexpected response from client" x)]))
   (define (finish-renderer self profile)
     (match-define (report-renderer/stream _ _ _ _ shutdown! channel) self)
     ; tell the connected client to wrap up
     (channel-put channel 'finish)
     ; wait for it to acknowledge, to give it time to pump its last messages
     (match (channel-get channel)
       ['finish (void)]
       [x (raise x)])
     ; now safe to shutdown the websocker server
     (shutdown!))])


;; Remove enter/exit events corresponding to calls that are too short to render.
;; This only removes enter/exit events that contain no intervening events, to
;; avoid pruning "interesting" calls.
;; events must not be '()
(define (prune-short-events events [min% 0.001])
  ; determine the minimum time for an event to be included
  (define (event->time evt)
    (metrics-ref (profile-event-metrics evt) 'time))
  (define (dt enter exit)
    (- (event->time exit)
       (event->time enter)))

  ; find the first and last events that have timestamps
  (define-values (first last)
    (let loop ([first #f][last #f][events events])
      (cond
        [(null? events) (values first last)]
        [(false? first)
         (if (false? (event->time (car events)))
             (loop first last (cdr events))
             (loop (car events) (car events) (cdr events)))]
        [else
         (if (false? (event->time (car events)))
             (loop first last (cdr events))
             (loop first (car events) (cdr events)))])))

  (define MIN_TIME (if (= (filtering-threshold) 0)
                       0
                       (* (dt first last) min%)))
  (define new-events '())

  (for ([e events])
    (cond
      [(and (profile-event-exit? e)
            (not (null? new-events)))
       (if (and (profile-event-enter? (car new-events))
                (< (dt (car new-events) e) MIN_TIME))
           (set! new-events (cdr new-events))
           (set! new-events (cons e new-events)))]
      [else (set! new-events (cons e new-events))]))

  (reverse new-events))


;; Initialize the template
(define (render-html-template source symlink?)
  ; set up output directory
  (define output-dir 
    (if symlink?
        (build-path (current-directory) "profiles" (make-folder-name source))
        (build-path (find-system-path 'temp-dir) (make-folder-name source))))
  (make-directory* output-dir)

  ; link/copy the template files into the output directory
  (define copy-or-symlink (if symlink?
                              make-file-or-directory-link
                              copy-directory/files))
  (let ([src (path->complete-path template-dir)])
    (for ([n (list "profile.html" "css" "js")])
      (copy-or-symlink (build-path src n) (build-path output-dir n))))

  output-dir)


;; A special type of message that contains the metadata
(define (metadata-message source name)
  (hash 'type "metadata"
        'name name
        'time (parameterize ([date-display-format 'iso-8601])
                (string-replace (date->string (current-date) #t) "T" " "))
        'source (syntax-srcloc source)
        'form (if (syntax? source) (~a (syntax->datum source)) "")
        'version 1))

;; A special type of message that tells the client to connect to a websocker
(define (stream-start-message port)
  (hash 'type "stream"
        'event "start"
        'url (format "ws://localhost:~v/" port)))

;; A special type of message that tells the client to close its websocket
(define (stream-finish-message)
  (hash 'type "stream" 'event "finish"))


;; Render a list of messages (which must be jsexpr?s) to the data file.
;; They're written in JSONP format, so will invoke the client's receiveData
;; callback when loaded.
(define (render-report-messages messages path)
  (let ([out (open-output-file (build-path path "report_data.js"))])
    (fprintf out "data.receiveData(")
    (write-json messages out)
    (fprintf out ");\n")
    (close-output-port out)))


;; Open the report in the system browser
(define (open-report-in-browser path)
  (define profile-path (build-path path "profile.html"))
  (printf "Wrote profile to: ~a\n" profile-path)
  (unless (getenv "SYMPRONOOPEN")
    (send-url/file profile-path)))


; Atomically replace the value in the box with a new value,
; and return the value that was previously there.
(define (unbox/replace! box new)
  (define v (unbox box))
  (if (box-cas! box v new) v (unbox/replace! box new)))
