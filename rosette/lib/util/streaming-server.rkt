#lang racket

(provide start-streaming-server)
(require racket/async-channel
         json
         "ws-server.rkt")

; Launch the WebSocket server
(define (start-streaming-server get-message interval get-finish-message
                                #:on-shutdown [on-shutdown #f])
  (define channel (make-channel))  ; channel for communicating with main thread
  (define connected? (box #f))  ; only one client may connect

  ; the procedure for handling connections
  (define (ws-connection conn _state)
    ; only one client may connect
    (cond
      [(box-cas! connected? #f #t)  ; we won the race to connect
       ; tell the main thread we're connected
       (channel-put channel 'connected)
       ; loop until we're done
       (let loop ()
         (define sync-result (sync/timeout/enable-break interval channel))
         (define messages (get-message))

         ; send the messages; bail out if it fails
         (define continue? (not (eq? sync-result 'finish)))
         (with-handlers ([exn:fail? (lambda (e)
                                      (eprintf "~e\n" e)
                                      (set! continue? #f))])
           (ws-send! conn (jsexpr->bytes messages)))
         (if continue?
             (loop)
             (begin
               (with-handlers ([exn:fail? void])  ; the connection might be dead, but we don't care
                 (ws-send! conn (jsexpr->bytes (list (get-finish-message))))
                 (ws-close! conn))
               ; if we weren't told to shut down, we need to wait until we are.
               (cond
                 [on-shutdown (on-shutdown)]
                 [else
                  (unless (eq? sync-result 'finish)
                    (channel-get channel))
                  (channel-put channel 'finish)]))))]
      [else
       (channel-put channel "another client is already connected")
       (ws-close! conn)]))

  ; start the server
  (define conf-channel (make-async-channel))
  (define server-shutdown!
    (ws-serve #:confirmation-channel conf-channel
              #:port 8048
              ws-connection))
  ; wait until it's started
  (define the-port (async-channel-get conf-channel))
  (unless (number? the-port)
    (raise the-port))

  (values the-port server-shutdown! channel))
