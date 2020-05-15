#lang racket

(require "../util/streaming-server.rkt"
         "sample-data.rkt")

(define the-thread (current-thread))

(let loop ()
  (define-values (_port shutdown! connected-channel)
    (start-streaming-server
     (thunk sample)
     2.0
     (thunk (hash 'type "shutdown" 'data 'null))
     #:on-shutdown (thunk (thread-send the-thread 'shutdown))))

  (println 'waiting-for-connection)
  (match (channel-get connected-channel)
    ['connected (void)]
    [x (error "unexpected response from client" x)])
  (println 'connected)
  (match (thread-receive)
    ['shutdown (shutdown!)]
    [_ (error 'impossible)])
  (loop))
